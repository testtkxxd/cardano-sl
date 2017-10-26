{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Server which handles transactions.

module Pos.Txp.Network.Listeners
       ( txRelays
       , txInvReqDataParams
       ) where

import qualified Data.HashMap.Strict       as HM
import           Data.Tagged               (Tagged (..), tagWith)
import           Formatting                (build, sformat, (%))
import           System.Wlog               (logInfo)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Binary.Relay          ()
import           Pos.Communication.Limits  ()
import           Pos.Communication.Message ()
import           Pos.Communication.Relay   (InvReqDataParams (..), MempoolParams (..),
                                            Relay (..))
import           Pos.Communication.Types   (MsgType (..))
import           Pos.Core.Txp              (TxAux (..), TxId)
import           Pos.Crypto                (hash)
import           Pos.Txp.MemState          (getMemPool, txpProcessTx)
import           Pos.Txp.Network.Types     (TxMsgContents (..))
import           Pos.Txp.Toil.Types        (MemPool (..))
import           Pos.Util.JsonLog          (JLEvent (..), JLTxR (..))
import           Pos.Util.TimeWarp         (CanJsonLog (..))
import           Pos.WorkMode.Class        (WorkMode)

txInvReqDataParams :: WorkMode ctx m
    => InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents m
txInvReqDataParams =
    InvReqDataParams
       { invReqMsgType = MsgTransaction
       , contentsToKey = txContentsToKey
       , handleInv = \_ -> txHandleInv
       , handleReq = \_ -> txHandleReq
       , handleData = \_ -> txHandleData
       }
  where
    txContentsToKey = pure . Tagged . hash . taTx . getTxMsgContents
    txHandleInv (Tagged txId) =
        not . HM.member txId  . _mpLocalTxs <$> getMemPool
    txHandleReq (Tagged txId) =
        fmap TxMsgContents . HM.lookup txId . _mpLocalTxs <$> getMemPool
    txHandleData (TxMsgContents txAux) = handleTxDo txAux

txRelays
    :: forall ctx m. WorkMode ctx m
    => [Relay m]
txRelays = pure $
    InvReqData (KeyMempool (Proxy :: Proxy TxMsgContents)
                           (map tag . HM.keys . _mpLocalTxs <$> getMemPool)) $
               txInvReqDataParams
  where
    tag = tagWith (Proxy :: Proxy TxMsgContents)

-- Real tx processing
-- CHECK: @handleTxDo
-- #txProcessTransaction
handleTxDo
    :: WorkMode ctx m
    => TxAux -> m Bool
handleTxDo txAux = do
    let txId = hash (taTx txAux)
    res <- txpProcessTx (txId, txAux)
    let json me = jsonLog $ JLTxReceived $ JLTxR
            { jlrTxId     = sformat build txId
            , jlrError    = me
            }
    case res of
        Right _ -> do
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
            json Nothing
            pure True
        Left er -> do
            logInfo $
                sformat ("Transaction hasn't been added to storage: "%build%" , reason: "%build) txId er
            json $ Just $ sformat build er
            pure False
