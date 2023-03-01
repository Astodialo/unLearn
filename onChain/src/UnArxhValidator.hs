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

module UnArxhValidator where


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

{-# INLINABLE unArxhValidator #-}
unArxhValidator :: () -> () -> Api.ScriptContext -> Bool
unArxhValidator _ _ ctx = traceIfFalse "moving the unmovable" unmovable

  where
    txInfo :: Api.TxInfo
    txInfo = Api.scriptContextTxInfo ctx

    unmovable :: Bool
    unmovable =  notElem "dianiko currency symbol" (map (\(cs,_ ,_) -> cs ) (flattenValue (Api.txInfoMint txInfo)))

