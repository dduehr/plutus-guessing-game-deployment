{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude (pure, putStrLn, show, (<$>))

import Data.Foldable (for_)
import Data.Text as T (Text, unpack)
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import System.IO (IO)

import qualified Cardano.Api as CApi
    ( Address
    , AsType(AsShelleyAddress)
    , PaymentCredential(PaymentCredentialByScript)
    , NetworkId(Testnet)
    , NetworkMagic(..)
    , PlutusScriptV1
    , Script
    , StakeAddressReference(NoStakeAddress)
    , ShelleyAddr
    , deserialiseAddress
    , hashScript
    , makeShelleyAddress
    , serialiseAddress
    , serialiseToCBOR
    , serialiseToRawBytes
    , writeFileTextEnvelope
    )

import qualified Ledger as PLedger (POSIXTime(..), PaymentPubKeyHash(..), PubKeyHash(..))
import qualified Ledger.Typed.Scripts as PScripts (Validator, validatorScript)
import qualified Plutus.V1.Ledger.Scripts as PV1Scripts (getValidator)
import qualified Plutus.Script.Utils.V1.Scripts as AV1Scripts (toCardanoApiScript)

import qualified Plutus.Contracts.Game as Game (GameParam(..), gameInstance)

-- The public key hash of "myAddress" is only used to personalize the guessing game
-- See https://github.com/input-output-hk/plutus-apps/blob/v1.1.0/plutus-use-cases/src/Plutus/Contracts/Game.hs#L113
myAddress = "addr_test1qp9c8usjvcpdk98d0jjzufyuyzkr85rre7srrwsz6tge0r09qv3544nfm67gn2km2r8zmd78yclfh2jyqe28yg8rcf4q8565vt"
gameStart = PLedger.POSIXTime 0 
networkId = CApi.Testnet $ CApi.NetworkMagic 2

-- Output directory for the CBOR hex representation of the guessing game validator script
outputDir = "dist"

main :: IO ()
main = do
    case mkGameParam myAddress gameStart of
        Just gameParam -> do
            let script = plutusScriptV1 $ mkValidator gameParam
            let scriptAddressBech32 = unpack . CApi.serialiseAddress $ mkScriptAddress networkId script
            let scriptTextEnvelopePath = outputDir <> "/" <> scriptAddressBech32 <> ".txt"
            putStrLn $ "Game address: " <> scriptAddressBech32
            putStrLn $ "Writing " <> scriptTextEnvelopePath <> " ..." 
            createDirectoryIfMissing True outputDir
            CApi.writeFileTextEnvelope scriptTextEnvelopePath Nothing script
            putStrLn "Done."
        _ -> do
            putStrLn "Failed to create game parameter."
    pure ()

deserialiseShelleyAddress :: Text -> Maybe (CApi.Address CApi.ShelleyAddr)
deserialiseShelleyAddress = CApi.deserialiseAddress CApi.AsShelleyAddress

paymentPubKeyHash :: CApi.Address CApi.ShelleyAddr -> PLedger.PaymentPubKeyHash
paymentPubKeyHash = PLedger.PaymentPubKeyHash . PLedger.PubKeyHash . toBuiltin . CApi.serialiseToRawBytes

mkGameParam :: Text -> PLedger.POSIXTime -> Maybe Game.GameParam
mkGameParam payeeAddress startTime = do
    payeePkh <- paymentPubKeyHash <$> deserialiseShelleyAddress payeeAddress
    pure $ Game.GameParam payeePkh startTime

mkValidator :: Game.GameParam -> PScripts.Validator
mkValidator = PScripts.validatorScript . Game.gameInstance

plutusScriptV1 :: PScripts.Validator -> CApi.Script CApi.PlutusScriptV1
plutusScriptV1 = AV1Scripts.toCardanoApiScript . PV1Scripts.getValidator

mkScriptAddress :: CApi.NetworkId -> CApi.Script CApi.PlutusScriptV1 -> CApi.Address CApi.ShelleyAddr
mkScriptAddress networkId script = CApi.makeShelleyAddress networkId (CApi.PaymentCredentialByScript $ CApi.hashScript script) CApi.NoStakeAddress

scriptAddressBech32 :: CApi.Address CApi.ShelleyAddr -> Text
scriptAddressBech32 = CApi.serialiseAddress