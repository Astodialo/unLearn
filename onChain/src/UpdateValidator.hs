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

module UpdateValidator where

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
import           Ledger.Constraints                                    as Constraints
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


data DatumMetadata = DatumMetadata { datumN      :: ![(BuiltinData, BuiltinData)],
                                     datumT      :: ![(BuiltinData, BuiltinData)],
                                     datumQ      :: ![(BuiltinData, BuiltinData)],
                                     datumA      :: ![(BuiltinData, BuiltinData)],
                                     datumR      :: ![(BuiltinData, BuiltinData)],
                                     datumState  :: ![(BuiltinData, BuiltinData)],
                                     datumAmount :: ![(BuiltinData, BuiltinData)]}


PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]

-- Validator that holds reference NFT with metadata
{-# INLINABLE propUpdateVal #-}
propUpdateVal :: DatumMetadata -> () -> Api.ScriptContext -> Bool
propUpdateVal dtm _ ctx = traceIfFalse "no mint/burn wallet signature" checkSign
                       && traceIfFalse "val nft not burnt or ref nft not locked in scr" checkBurnLock

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    checkSign :: Bool
    checkSign = "5867c3b8e27840f556ac268b781578b14c5661fc63ee720dbeab663f" `elem` map getPubKeyHash (Api.txInfoSignatories txInfo)

    listValEq :: [(CurrencySymbol, TokenName, Integer)] -> Bool
    listValEq val = case val of
        [(cs,tkn, _), (cs', tkn', _)] -> cs == cs' && (unTokenName tkn == (unTokenName tkn') `appendByteString` "_A" || unTokenName tkn == (unTokenName tkn') `appendByteString` "_R")
        otherwise                             -> False

    txMint :: [(CurrencySymbol, TokenName, Integer)]
    txMint = flattenValue (Api.txInfoMint txInfo)

    valHashBS :: Api.TxOut -> ValidatorHash
    valHashBS infout = case Api.addressCredential (Api.txOutAddress infout) of
        (Api.ScriptCredential vh) -> vh
        (Api.PubKeyCredential _)  -> ValidatorHash "error"

    checkLockedRefTkn :: Api.TxOut -> Bool
    checkLockedRefTkn infout = valHashBS infout == Api.ownHash ctx && (any (\(cs, _, _) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" ) $ flattenValue (Api.txOutValue infout))

    lockedRefTkn :: [(CurrencySymbol, TokenName, Integer)]
    -- lockedRefTkn = concat (flattenValue `map` (Api.txOutValue `map` (filter checkLockedRefTkn (Api.txInfoOutputs txInfo))))
    lockedRefTkn = concat $ flattenValue `map` (Api.txOutValue `map` (Api.txInfoOutputs txInfo))

    burnedValidationTkn :: [(CurrencySymbol, TokenName, Integer)]
    burnedValidationTkn = filter (\(cs,_,amt) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" && amt == -1) txMint

    checkBurnLock :: Bool
    checkBurnLock = listValEq $ lockedRefTkn ++ burnedValidationTkn


data Typed
instance TScripts.ValidatorTypes Typed where
  type instance RedeemerType Typed =()
  type instance DatumType Typed = DatumMetadata

tvalidator :: TScripts.TypedValidator Typed
tvalidator = TScripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| propUpdateVal ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = TScripts.mkUntypedValidator @DatumMetadata @()

validator :: Scripts.Validator
validator = TScripts.validatorScript tvalidator

valHash :: Scripts.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddr :: Address
scrAddr = TScripts.validatorAddress tvalidator

-- Write Stuff

script :: Api.Script
script = Api.unValidatorScript validator

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "testnet/lockValidator.plutus" Nothing serialisedScript

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . Api.toData


