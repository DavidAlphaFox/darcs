{-# LANGUAGE ScopedTypeVariables #-}
import Storage.Hashed.Test( tests )
import Prelude hiding( catch )
import Test.Framework( defaultMain )
import System.Directory( createDirectory, removeDirectoryRecursive
                       , setCurrentDirectory )
import Codec.Archive.Zip( extractFilesFromArchive, toArchive )
import qualified Data.ByteString.Lazy as BL
import Control.Exception( catch, IOException )

main :: IO ()
main = do zipFile <- toArchive `fmap` BL.readFile "hashed-storage/testdata.zip"
          removeDirectoryRecursive "_test_playground" `catch` \(_ :: IOException) -> return ()
          createDirectory "_test_playground"
          setCurrentDirectory "_test_playground"
          extractFilesFromArchive [] zipFile
          defaultMain tests
