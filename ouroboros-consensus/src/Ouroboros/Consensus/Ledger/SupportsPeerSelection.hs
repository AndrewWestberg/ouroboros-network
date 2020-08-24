module Ouroboros.Consensus.Ledger.SupportsPeerSelection (
    LedgerSupportsPeerSelection (..)
    -- * Re-exports for convenience
  , DomainAddress (..)
  , Domain
  , PortNumber
  ) where

import           Ouroboros.Network.PeerSelection.RootPeersDNS (Domain,
                     DomainAddress (..), PortNumber)

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState)

class LedgerSupportsPeerSelection blk where
  -- | Return peers registered in the ledger ordered by descending some
  -- /preference/.
  --
  -- For example, for Shelley, this should return the stake pool relays that
  -- have been registered. The /preference/ should be the /stake/, i.e., the
  -- stake pools with the most stake will come first in the list.
  getPeers :: LedgerState blk -> [DomainAddress]
