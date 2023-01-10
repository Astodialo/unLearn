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

module Minting where

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

import           Plutus.Contract

import           Ledger
import           Ledger.Constraints                                    as Constraints
import           Ledger.Value                                          as Value

import qualified Plutus.Script.Utils.V2.Scripts                        as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts                  as TScripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as Scripts
import           Plutus.V1.Ledger.Value                                as V
import qualified Plutus.V2.Ledger.Api                                  as Api
import           Plutus.V2.Ledger.Contexts                             as Api

import           Plutus.Trace
import qualified Plutus.Trace.Emulator                                 as Emulator
import qualified Wallet.Emulator.Wallet                                as Wallet

import qualified PlutusTx
import           PlutusTx.Prelude                                      as Plutus hiding
                                                                                 (Semigroup (..),
                                                                                  unless,
                                                                                  (.))

import           Control.Monad.Freer.Extras                            as Extras
import           Data.List                                             (concat,
                                                                        concatMap,
                                                                        filter,
                                                                        groupBy)
import           Data.Text                                             (Text)
import           Prelude                                               (FilePath,
                                                                        IO,
                                                                        Semigroup (..),
                                                                        Show (..),
                                                                        String,
                                                                        print,
                                                                        (.))


data DatumProp = DatumProp { pName      :: !BuiltinByteString
                           --, ppPrev      :: Api.TxOutRef
                           , pState     :: !BuiltinByteString
                           , pQuestions :: !BuiltinByteString
                           , pAnswers   :: ![BuiltinByteString]
                           , pResults   :: ![(Integer, BuiltinByteString)]
                           } deriving (Generic,ToJSON, FromJSON, Show)

PlutusTx.makeLift ''DatumProp
PlutusTx.makeIsDataIndexed ''DatumProp [('DatumProp,0)]

{-# INLINABLE propUpdateVal #-}
propUpdateVal :: DatumProp -> () -> Api.ScriptContext -> Bool
propUpdateVal dp _ ctx = traceIfFalse "no mint/burn wallet signiature" checkSign &&
                         traceIfFalse "no nft pair" checkNfts

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    valEq :: (CurrencySymbol, TokenName, Integer) -> (CurrencySymbol, TokenName, Integer) -> Bool
    valEq = \(cs,tkn, _) (cs', tkn', _) -> cs == cs' && unTokenName tkn == (unTokenName tkn' <> "_A")

    listValEq :: [(CurrencySymbol, TokenName, Integer)] -> Bool
    listValEq = \[(cs,tkn, _), (cs', tkn', _)] -> cs == cs' && unTokenName tkn == (unTokenName tkn' <> "_A")

    txMint :: [(CurrencySymbol, TokenName, Integer)]
    txMint = flattenValue (Api.txInfoMint txInfo)

    checkLockedRefTkn :: Api.TxOut -> Bool
    checkLockedRefTkn infout = (Api.ScriptCredential (Api.addressCredential (Api.txOutAddress infout))) == Api.ownHash ctx && (any (\(cs, _, _) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" ) $ flattenValue (Api.txOutValue infout))

    lockedRefTkn :: [(CurrencySymbol, TokenName, Integer)]
    lockedRefTkn = Data.List.concat (flattenValue `map` (Api.txOutValue `map` (Data.List.filter checkLockedRefTkn (Api.txInfoOutputs txInfo))))

    burnedValidationTkn :: [(CurrencySymbol, TokenName, Integer)]
    burnedValidationTkn = Data.List.filter (\(cs,_,amt) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" && amt == -1) txMint

    checkSign:: Bool
    checkSign = "5867c3b8e27840f556ac268b781578b14c5661fc63ee720dbeab663f" `elem` map getPubKeyHash (Api.txInfoSignatories txInfo)

    checkBurnLock :: Bool
    checkBurnLock = valEq (head lockedRefTkn) (head burnedValidationTkn)

    txWinValues :: [(CurrencySymbol, TokenName, Integer)]
    txWinValues = Data.List.filter (\(cs, _, _) -> cs == CurrencySymbol "d9312da562da182b02322fd8acb536f37eb9d29fba7c49dc17255527" ) $ Data.List.concat (Data.List.filter ((/=1) . length) . (groupBy valEq) $ Data.List.concat $ flattenValue `map` (Api.txOutValue `map` (Api.txInInfoResolved `map` Api.txInfoInputs txInfo)))

    checkNfts :: Bool
    checkNfts =  listValEq txWinValues

data Typed
instance TScripts.ValidatorTypes Typed where
  type instance DatumType Typed = DatumProp
  type instance RedeemerType Typed = ()

tvalidator :: TScripts.TypedValidator Typed
tvalidator = TScripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| propUpdateVal ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
          wrap = TScripts.mkUntypedValidator @DatumProp @()

validator :: Scripts.Validator
validator = TScripts.validatorScript tvalidator

valHash :: Scripts.ValidatorHash
valHash = TScripts.validatorHash tvalidator

scrAddr :: Address
scrAddr = TScripts.validatorAddress tvalidator

-- Write Stuff

redeemerTest :: DatumProp
redeemerTest = DatumProp { pName= "0001",
                           pState = "Init",
                           pQuestions = "Does it work?",
                           pAnswers = [],
                           pResults = []
                          }

printRedeemer :: IO ()
printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ Api.toData redeemerTest)

script :: Api.Script
script = Api.unValidatorScript validator

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "testnet/nftMint.plutus" Nothing serialisedScript

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . Api.toData

writeRedeemer :: IO ()
writeRedeemer = writeJSON "testnet/redeemer.json" ()


