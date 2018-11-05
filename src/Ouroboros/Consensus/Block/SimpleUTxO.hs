{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Block.SimpleUTxO where

import qualified Data.Hashable as Hashable
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Natural (naturalToWordMaybe)

import qualified Ouroboros.Consensus.Crypto.Hash as H
import           Ouroboros.Consensus.Crypto.Hash.MD5 (MD5)
import           Ouroboros.Consensus.Ledger.Mock (HasUtxo (..))
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Util (Condense (..))
import           Ouroboros.Network.Block
import           Ouroboros.Network.Serialise
import           Ouroboros.Network.Testing.ConcreteBlock

-- Concrete block representation with a bunch of transactions
--
-- For now we just reuse the concrete header from the networking layer.
data SimpleUtxoBlock = SimpleUtxoBlock {
      simpleUtxoHeader :: BlockHeader
    , simpleUtxoBody   :: Set Mock.Tx
    }
  deriving (Generic, Show)

instance Condense SimpleUtxoBlock where
  condense (SimpleUtxoBlock hdr bdy) =  "("  <> condenseBlockHeader hdr
                                     <> ", " <> condense bdy
                                     <> ")))"
    where
      condenseBlockHeader :: BlockHeader -> String
      condenseBlockHeader BlockHeader{..} =
          let (HeaderHash hsh)  = headerHash
              prevH = case headerPrevHash of
                           GenesisHash              ->
                             Hashable.hash (GenesisHash :: Hash SimpleUtxoBlock)
                           BlockHash (HeaderHash b) ->
                             b
              (BlockSigner sig) = headerSigner
          -- Grab the absolute value in case of negative integers, only for
          -- the sake of having a slightly more readable output. This will go
          -- away once we will transition to short hashes or to use 'Hash'
          -- from the Crypto module.
          in show (abs hsh) <>
             "-" <> show (abs prevH) <>
             ",(" <> show sig <>
             ",(" <> show (getSlot headerSlot)

simpleUtxoBodyHash :: Set Mock.Tx -> BodyHash
simpleUtxoBodyHash = BodyHash
                   . maybe 0 fromIntegral
                   . naturalToWordMaybe
                   . H.fromHash
                   . (H.hash @MD5)
                   . Set.toList

instance HasUtxo SimpleUtxoBlock where
  txIns      = txIns      . simpleUtxoBody
  txOuts     = txOuts     . simpleUtxoBody
  updateUtxo = updateUtxo . simpleUtxoBody
  confirmed  = confirmed  . simpleUtxoBody

instance StandardHash SimpleUtxoBlock

instance HasHeader SimpleUtxoBlock where
    type HeaderHash SimpleUtxoBlock = ConcreteHeaderHash

    blockHash      =            headerHash     . simpleUtxoHeader
    blockPrevHash  = castHash . headerPrevHash . simpleUtxoHeader
    blockSlot      =            headerSlot     . simpleUtxoHeader
    blockNo        =            headerBlockNo  . simpleUtxoHeader

    -- | The block invariant is just that the actual block body hash matches the
    -- body hash listed in the header.
    --
    blockInvariant (SimpleUtxoBlock header body) =
        simpleUtxoBodyHash body == headerBodyHash header

instance Serialise SimpleUtxoBlock where
  -- rely on generics instance
