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

import           Conduit ( MonadThrow )
import           Control.Monad.State.Lazy ( StateT, get, lift, put)
import           Data.ByteString ( ByteString, uncons )
import qualified Data.ByteString as BS
import           Data.ByteString.Short ( toShort )
import           Data.Conduit ( ConduitT, (.|), await, awaitForever, yield )
import           Data.Conduit.Lift ( evalStateLC )
import           Data.Conduit.Tar
                   ( FileInfo, Header (..), TarChunk (..), untarChunks
                   , withFileInfo
                   )
import           Data.Map ( Map )
import qualified Data.Map as Map

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
  Just (ChunkPayload _ b) -> pure $ paxParser b
  _ -> pure mempty

-- | A pax extended header comprises one or more records. If the pax extended
-- header is empty or does not parse, yields an empty 'Pax'.
paxParser :: ByteString -> Pax
paxParser b
  -- This is an error case.
  | BS.null b = mempty
paxParser b = paxParser' [] b
 where
  paxParser' :: [(ByteString, ByteString)] -> ByteString -> Pax
  paxParser' l b0
    | BS.null b0 = Map.fromList l
  paxParser' l b0 =
    maybe mempty (\(pair, b1) -> paxParser' (pair:l) b1) (recordParser b0)

-- | A record in a pax extended header has format:
--
-- "%d %s=%s\n", <length>, <keyword>, <value>
--
-- If the record does not parse @(<keyword>, <value>)@, yields 'Nothing'.
recordParser :: ByteString -> Maybe ((ByteString, ByteString), ByteString)
recordParser b0 = do
  let (nb, b1) = BS.span isDecimal b0
  n <- if nb == mempty
    then Nothing
    else Just $ parseDecimal nb
  (sb, b2) <- uncons b1
  _ <- skip isSpace sb
  let (k, b3) = BS.span (not . isEquals) b2
  (eb, b4) <- uncons b3
  _ <- skip isEquals eb
  let (v, b5) = BS.splitAt (n - BS.length nb - BS.length k - 3) b4
  (fb, b6) <- uncons b5
  _ <- skip isNewline fb
  Just ((k, v), b6)
 where
  newline = 0x0a -- UTF-8 '\n'
  space = 0x20 -- UTF-8 ' '
  zero = 0x30 --  UTF-8 '0'
  nine = 0x39 -- UTF-8 '9'
  equals = 0x3d -- UTF-8 '='
  isDecimal w = w >= zero && w <= nine
  parseDecimal :: Integral i => ByteString -> i
  parseDecimal = BS.foldl' (\t c -> t * 10 + fromIntegral (c - zero)) 0
  skip p w = if p w then Just () else Nothing
  isSpace = (space ==)
  isEquals = (equals ==)
  isNewline = (newline ==)
