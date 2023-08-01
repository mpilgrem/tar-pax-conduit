{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | See the extended description of the pax interchange format at:
-- https://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html. This
-- module provides a 'Data.Conduit.ConduitT' that consumes a stream of
-- 'Data.Conduit.Tar.TarChunk' which may include pax header blocks and produces
-- a stream of 'Data.Conduit.Tar.TarChunk' to which pax extended headers have
-- been applied.
module Data.Conduit.Tar.Pax
  ( untar'
  , PaxState
  , initialPaxState
  , applyPaxChunkHeaders
  ) where

import           Data.Attoparsec.ByteString as AP
                   ( Parser, many1, match, parseOnly, skip, take, takeTill )
import           Data.Attoparsec.ByteString.Char8 ( decimal )
import           Data.Conduit ( ConduitT, (.|), await, awaitForever, yield )
import           Data.Conduit.Lift ( evalStateLC )
import           Data.Conduit.Tar
                   ( FileInfo, Header (..), TarChunk (..), untarChunks
                   , withFileInfo
                   )
import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.Map as Map
import           RIO.State ( StateT, get, put )

-- | Type synonym representing a pax extended header.
type Pax = Map ByteString ByteString

-- | Type representing states given pax extended headers.
data PaxState = PaxState Pax Pax

-- | Like 'Data.Conduit.Tar.untar' but applies pax header blocks.
untar' ::
     MonadThrow m
  => (FileInfo -> ConduitT ByteString o m ())
  -> ConduitT ByteString o m ()
untar' inner =
     untarChunks
  .| evalStateLC initialPaxState applyPaxChunkHeaders
  .| withFileInfo inner

-- | The initial state before applying any pax extended headers.
initialPaxState :: PaxState
initialPaxState = PaxState mempty mempty

-- | Applies tar chunks that are pax extended headers to the tar chunks that
-- follow. However, only the 'comment', 'linkpath' and 'path' keywords are
-- supported.
applyPaxChunkHeaders ::
     Monad m
  => ConduitT TarChunk TarChunk (StateT PaxState m) ()
applyPaxChunkHeaders = awaitForever $ \i -> do
  state@(PaxState g x) <- lift get
  let updateState f = do
        p <- parsePax
        lift $ put $ f p state
        pure ()
  case i of
    ChunkHeader h -> case headerLinkIndicator h of
      -- 'g'
      0x67 -> updateState updateGlobal
      -- 'x'
      0x78 -> updateState updateNext
      _ -> do
        yield $ ChunkHeader $ applyPax (Map.union x g) h
        lift $ put $ clearNext state
    _ -> yield i
 where
  updateGlobal p (PaxState g x) = PaxState (Map.union p g) x
  updateNext p (PaxState g _) = PaxState g p
  clearNext = updateNext mempty

-- | Only the 'comment', 'linkpath' and 'path' keywords are supported.
applyPax :: Pax -> Header -> Header
applyPax p h = updateLinkpath $ updatePath h
 where
  -- The 'comment' keyword is ignored.
  updateLinkpath = case p Map.!? "linkpath" of
    Nothing -> id
    Just fp -> \h' -> h' { headerLinkName = toShort fp }
  updatePath = case p Map.!? "path" of
    Nothing -> id
    Just fp -> \h' -> h'
      { headerFileNameSuffix = toShort fp
      , headerFileNamePrefix = mempty
      }

parsePax :: Monad m => ConduitT TarChunk TarChunk (StateT PaxState m) Pax
parsePax = await >>= \case
  Just (ChunkPayload _ b) ->
    pure $ either (const mempty) id (parseOnly paxParser b)
  _ -> pure mempty

-- | A pax extended header comprises one or more records.
paxParser :: Parser Pax
paxParser = Map.fromList <$> AP.many1 recordParser

-- | A record in a pax extended header has format:
--
-- "%d %s=%s\n", <length>, <keyword>, <value>
recordParser :: Parser (ByteString, ByteString)
recordParser = do
  (nb, n) <- match decimal
  skip (== 0x20) -- UTF-8 ' '
  (kb, k) <- match $ takeTill (== 0x3d) -- UTF-8 '='
  skip (== 0x3d) -- UTF-8 '='
  v <- AP.take (n - BS.length nb - BS.length kb - 3)
  skip (== 0x0A) -- UTF-8 '\n'
  pure (k, v)
