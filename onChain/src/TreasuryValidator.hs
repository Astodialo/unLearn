{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TreasuryValidator where


import           Cardano.Api                                           (PlutusScript,
                                                                        PlutusScriptV2,
                                                                        writeFileTextEnvelope)
import           Cardano.Api.Shelley                                   (PlutusScript (..),
                                                                        ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                                        fromPlutusData,
                                                                        scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                                            as A
import qualified Data.ByteString.Lazy                                  as LBS
import qualified Data.ByteString.Short                                 as SBS
import           Data.Functor                                          (void)
import           GHC.Generics                                          (Generic)

import           Ledger
import           Ledger.Ada                                            as Ada
import           Ledger.Constraints                                    as Constraints
import           Ledger.Constraints.OnChain.V2                         as OnChain
import           Ledger.Value                                          as Value

import qualified Plutus.Script.Utils.V2.Scripts                        as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts                  as TScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Scripts
import           Plutus.V1.Ledger.Address                              as Addr
import           Plutus.V1.Ledger.Value                                as V
import qualified Plutus.V2.Ledger.Api                                  as Api
import           Plutus.V2.Ledger.Contexts                             as Api

import           PlutusTx
import           PlutusTx.Prelude                                      as Plutus hiding
                                                                                 (Semigroup (..),
                                                                                  unless,
                                                                                  (.))

import           Data.Text                                             (Text)
import           Prelude                                               (FilePath,
                                                                        IO,
                                                                        Semigroup (..),
                                                                        Show (..),
                                                                        String,
                                                                        print,
                                                                        (.))

treasurVal :: BuiltinData -> BuiltinData -> Api.ScriptContext -> Bool
treasurVal _ _ ctx = traceIfFalse "no mint/burn wallet signature" checkSign
                  && traceIfFalse "wrong amount" payOutCheck

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    txMint :: [(CurrencySymbol, TokenName, Integer)]
    txMint = flattenValue (Api.txInfoMint txInfo)

    checkSign :: Bool
    checkSign = "5867c3b8e27840f556ac268b781578b14c5661fc63ee720dbeab663f" `elem` map getPubKeyHash (Api.txInfoSignatories txInfo)

    valEq :: (CurrencySymbol, TokenName, Integer) -> [(CurrencySymbol, TokenName, Integer)] -> Bool
    valEq (cs, tkn, _) val =  any (\(cs', tkn', _) -> cs == cs' && (unTokenName tkn == (unTokenName tkn') `appendByteString` "_Claim")) val

    burnAss :: (CurrencySymbol, TokenName, Integer)
    burnAss = head $ filter (\(cs, _, amt) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" && amt == -1) txMint

    valHashBS :: Addr.Address -> ValidatorHash
    valHashBS  addr = case Api.addressCredential addr of
        (Api.ScriptCredential vh) -> vh
        (Api.PubKeyCredential _)  -> ValidatorHash "error"

    refInput :: Api.TxOut
    refInput = head $ filter (\(Api.TxOut addr val _ _) -> valHashBS addr == "update Validator Addr" && valEq burnAss (flattenValue val)) (Api.txInInfoResolved `map` (Api.txInfoInputs txInfo))

    refDatum :: BuiltinData
    refDatum = case refInput of
        Api.TxOut _ _ outputDtm _-> case outputDtm of
            Api.OutputDatum dtm -> getDatum dtm

    dtmAmount :: Data
    dtmAmount = case refDatum of
       Api.BuiltinData (Constr _ [_, _, _, _, _, _, datumAmount]) -> (\(Map [(_, amt)]) -> amt) datumAmount

    amount :: Integer
    amount = case dtmAmount of
        I amt -> amt
        _     -> 0

    receiverAddr :: PubKeyHash
    receiverAddr = head $ filter (/="5867c3b8e27840f556ac268b781578b14c5661fc63ee720dbeab663f") (Api.txInfoSignatories txInfo)

    payOutConstraint :: TxConstraints () ()
    payOutConstraint = mustPayToPubKey (PaymentPubKeyHash receiverAddr) (lovelaceValueOf (amount))

    payOutCheck :: Bool
    payOutCheck = OnChain.checkScriptContext payOutConstraint ctx


