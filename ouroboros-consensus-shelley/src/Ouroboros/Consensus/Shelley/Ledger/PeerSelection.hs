{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.PeerSelection () where

import           Data.Bifunctor (second)
import           Data.Foldable (toList)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))

import           Ouroboros.Consensus.Ledger.SupportsPeerSelection

import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

instance LedgerSupportsPeerSelection (ShelleyBlock c) where
  getPeers ShelleyLedgerState { shelleyState } = concat
      [ Map.findWithDefault [] stakePool poolDomainAddresses
      | stakePool <- orderByStake poolDistr
      ]
    where
      poolDistr :: SL.PoolDistr c
      poolDistr = SL.nesPd shelleyState

      -- | Sort stake pools by descending stake
      orderByStake :: SL.PoolDistr c -> [SL.KeyHash 'SL.StakePool c]
      orderByStake =
            map fst
          . sortOn (Down . snd)
          . map (second SL.individualPoolStake)
          . Map.toList
          . SL.unPoolDistr

      -- | Note that a stake pool can have multiple registered relays
      poolDomainAddresses :: Map (SL.KeyHash 'SL.StakePool c) [DomainAddress]
      poolDomainAddresses =
            Map.map (map relayToDomainAddress . toList . SL._poolRelays)
          . SL._pParams
          . SL._pstate
          . SL._delegationState
          . SL.esLState
          . SL.nesEs
          $ shelleyState

      relayToDomainAddress :: SL.StakePoolRelay -> DomainAddress
      relayToDomainAddress = undefined
