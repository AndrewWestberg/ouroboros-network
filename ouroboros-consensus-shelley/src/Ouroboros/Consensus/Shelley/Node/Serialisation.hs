{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Binary (fromCBOR, toCBOR)
import           Codec.Serialise (decode, encode)

import           Ouroboros.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Storage.Serialisation

import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import           Ouroboros.Consensus.Shelley.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance Era era => HasBinaryBlockInfo (ShelleyBlock era) where
  getBinaryBlockInfo = shelleyBinaryBlockInfo

instance Era era => SerialiseDiskConstraints (ShelleyBlock era)

instance Era era => EncodeDisk (ShelleyBlock era) (ShelleyBlock era) where
  encodeDisk _ = encodeShelleyBlock
instance Era era => DecodeDisk (ShelleyBlock era) (Lazy.ByteString -> ShelleyBlock era) where
  decodeDisk _ = decodeShelleyBlock

instance Era era => EncodeDisk (ShelleyBlock era) (Header (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyHeader
instance Era era => DecodeDisk (ShelleyBlock era) (Lazy.ByteString -> Header (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyHeader

instance Era era => EncodeDisk (ShelleyBlock era) (LedgerState (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyLedgerState
instance Era era => DecodeDisk (ShelleyBlock era) (LedgerState (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyLedgerState

-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance Era era => EncodeDisk (ShelleyBlock era) (TPraosState era) where
  encodeDisk _ = encode
-- | @'ChainDepState' ('BlockProtocol' ('ShelleyBlock' era))@
instance Era era => DecodeDisk (ShelleyBlock era) (TPraosState era) where
  decodeDisk _ = decode

instance Era era => EncodeDisk (ShelleyBlock era) (AnnTip (ShelleyBlock era)) where
  encodeDisk _ = encodeShelleyAnnTip
instance Era era =>  DecodeDisk (ShelleyBlock era) (AnnTip (ShelleyBlock era)) where
  decodeDisk _ = decodeShelleyAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance Era era => SerialiseNodeToNodeConstraints (ShelleyBlock era)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance Era era => SerialiseNodeToNode (ShelleyBlock era) (ShelleyBlock era) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode (ShelleyBlock era) (Serialised (ShelleyBlock era))
  -- Default instance

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
instance Era era => SerialiseNodeToNode (ShelleyBlock era) (Header (ShelleyBlock era)) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeShelleyHeader
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeShelleyHeader

-- | We use CBOR-in-CBOR
instance SerialiseNodeToNode (ShelleyBlock era) (SerialisedHeader (ShelleyBlock era)) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

-- | The @To/FromCBOR@ instances defined in @cardano-ledger-specs@ use
-- CBOR-in-CBOR to get the annotation.
instance Era era => SerialiseNodeToNode (ShelleyBlock era) (GenTx (ShelleyBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

instance Era era => SerialiseNodeToNode (ShelleyBlock era) (GenTxId (ShelleyBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance Era era => SerialiseNodeToClientConstraints (ShelleyBlock era)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance Era era => SerialiseNodeToClient (ShelleyBlock era) (ShelleyBlock era) where
  encodeNodeToClient _ _ = wrapCBORinCBOR   encodeShelleyBlock
  decodeNodeToClient _ _ = unwrapCBORinCBOR decodeShelleyBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient (ShelleyBlock era) (Serialised (ShelleyBlock era))
  -- Default instance

-- | Uses CBOR-in-CBOR in the @To/FromCBOR@ instances to get the annotation.
instance Era era => SerialiseNodeToClient (ShelleyBlock era) (GenTx (ShelleyBlock era)) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

-- | @'ApplyTxErr' '(ShelleyBlock era)'@
instance Era era => SerialiseNodeToClient (ShelleyBlock era) (SL.ApplyTxError era) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

instance Era era => SerialiseNodeToClient (ShelleyBlock era) (SomeBlock Query (ShelleyBlock era)) where
  encodeNodeToClient _ _ (SomeBlock q) = encodeShelleyQuery q
  decodeNodeToClient _ _               = decodeShelleyQuery

instance Era era => SerialiseResult (ShelleyBlock era) (Query (ShelleyBlock era)) where
  encodeResult _ _ = encodeShelleyResult
  decodeResult _ _ = decodeShelleyResult

{-------------------------------------------------------------------------------
  HFC support

  Since 'NestedCtxt' for Shelley is trivial, these instances can use defaults.
-------------------------------------------------------------------------------}

instance Era era => ReconstructNestedCtxt Header (ShelleyBlock era)
instance Era era => EncodeDiskDepIx (NestedCtxt Header) (ShelleyBlock era)
instance Era era => EncodeDiskDep   (NestedCtxt Header) (ShelleyBlock era)
instance Era era => DecodeDiskDepIx (NestedCtxt Header) (ShelleyBlock era)
instance Era era => DecodeDiskDep   (NestedCtxt Header) (ShelleyBlock era)
