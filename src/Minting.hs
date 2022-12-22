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

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise

import           Data.Aeson                           as A
import           Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Utils.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (ownCurrencySymbol)
import qualified PlutusTx
import           PlutusTx.Prelude                     as Plutus hiding
                                                                (Semigroup (..),
                                                                 unless, (.))
import           Prelude                              (FilePath, IO,
                                                       Semigroup (..),
                                                       Show (..), String, print,
                                                       (.))

data PropParams = PropParams { ppIndex     :: Integer
                             --, ppPrev      :: PlutusV2.TxOutRef
                             , ppState     :: BuiltinByteString
                             , ppQuestions :: BuiltinByteString
                             , ppAnswers   :: [BuiltinByteString]
                             , ppResults   :: [(Integer, BuiltinByteString)]
                             } deriving Show

PlutusTx.makeLift ''PropParams
PlutusTx.unstableMakeIsData ''PropParams

{-# INLINABLE exColateralPolicy #-}
exColateralPolicy :: () -> PropParams -> PlutusV2.ScriptContext -> Bool
exColateralPolicy _ pp ctx = traceIfFalse "Wrong Redeemer!" $ checkRedeemer pp

  where
    checkRedeemer :: PropParams -> Bool
    checkRedeemer (PropParams _ s _ a r) = s == "Init" && a == [] && r == []

data Typed
instance Utils.V2.ValidatorTypes Typed where
  type instance DatumType Typed = ()
  type instance RedeemerType Typed = PropParams

tvalidator :: Utils.V2.TypedValidator Typed
tvalidator = Utils.V2.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| exColateralPolicy ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
          wrap = Utils.V2.mkUntypedValidator @() @PropParams

policy :: Utils.V2.MintingPolicy
policy = Utils.V2.forwardingMintingPolicy tvalidator

curSymbol :: CurrencySymbol
curSymbol = PSU.V2.scriptCurrencySymbol policy

--Write Stuff

redeemerTest :: PropParams
redeemerTest = PropParams { ppIndex = 1,
                            ppState = "Init",
                            ppQuestions = "Does it work?",
                            ppAnswers = [],
                            ppResults = []
                          }

printRedeemer :: IO ()
printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemerTest)

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript policy

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "testnet/nftMint.plutus" Nothing serialisedScript

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . A.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusV2.toData

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()


