module Main (main) where

import           Conduit ( sourceFile, sinkNull )
import           Data.Conduit ( (.|), runConduitRes )
import           Data.Conduit.Tar ( restoreFileInto )
import           Data.Conduit.Tar.Pax ( untar' )
import           System.Directory ( createDirectoryIfMissing )

main :: IO ()
main = do
  createDirectoryIfMissing True "output"
  runConduitRes $ do
       -- The test source file was created using the git archive command.
       sourceFile "test/longname.pax"
    .| untar' (restoreFileInto "output")
    .| sinkNull
