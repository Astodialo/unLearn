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
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), String, print,
                                                       (.))

data PPState = Init | Open | Closed | Canceled deriving (Show, Eq)

PlutusTx.unstableMakeIsData ''PPState

data PropParams = PropParams { ppState     :: PPState
                             , ppQuestions :: BuiltinByteString
                             , ppAnswers   :: [BuiltinByteString]
                             , ppResults   :: [(Integer, BuiltinByteString)]
                             } deriving (Show)

PlutusTx.unstableMakeIsData ''PropParams

{-# INLINABLE exColateralPolicy #-}
exColateralPolicy :: PropParams -> PlutusV2.ScriptContext -> Bool
exColateralPolicy pp ctx = traceIfFalse "Wrong Redeemer!" $ checkRedeemer pp

  where
    checkRedeemer :: PropParams -> Bool
    checkRedeemer (PropParams s _ a r) = s == Init && a == [] && r == []
