-- | This module defines functions to read Java JAR files.
module Java.JAR.Archive where

{--
import Codec.Archive.Zip
import Data.Binary
import Data.List
import qualified Data.ByteString.Lazy as B
import System.FilePath

import Java.ClassPath.Types
import Java.ClassPath.Common
import JVM.ClassFile
import JVM.Converter

readJAREntry :: (Enum a) => FilePath -> String -> IO (Maybe [a])
readJAREntry jarfile path = do
  catchZipError (Just `fmap` (withArchive [] jarfile $ fileContents [] path))
                    (\_ -> return Nothing)

-- | Read all entires from JAR file
readAllJAR :: FilePath -> IO [Tree CPEntry]
readAllJAR jarfile = do
    files <- withArchive [] jarfile $ fileNames []
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good file = ".class" `isSuffixOf` file

-- | Read one class from JAR file
readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarfile path = do
  content <- withArchive [] jarfile $ fileContents [] path
  let bstr = B.pack content
  return $ classFile2Direct (decode bstr)

checkClassTree :: [Tree CPEntry] -> IO [Tree (FilePath, Class Direct)]
checkClassTree forest = mapFMF check forest
  where
    check _ (NotLoaded path) = do
       cls <- parseClassFile path
       return (path, cls)
    check a (Loaded path cls) = return (a </> path, cls)
    check a (NotLoadedJAR jar path) = do
       cls <- readFromJAR jar (a </> path)
       return (a </> path, cls)
    check a (LoadedJAR _ cls) =
       return (a </> show (thisClass cls), cls)

zipJAR :: [Tree (FilePath, Class Direct)] -> Codec.Archive.Zip.ZipArchive ()
zipJAR forest = do
    mapFM go forest
    return ()
  where
    go (path, cls) = addFile path =<< sourceBuffer (B.unpack $ encodeClass cls)
--}