{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}



module Main where
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson                   (encode, toJSON, ToJSON)
import qualified Data.ByteString.Char8        as BS
import           Data.ByteString.Lazy         (toStrict)
import           Data.Conduit
import qualified Data.Conduit.Binary          as B
import qualified Data.Conduit.List            as CL
import qualified Data.Foldable                as F
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Typeable
import           Lib
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Write


data ProgramException = InputException String
                      deriving (Show, Typeable)

instance Exception ProgramException


parseMainArgs :: [String] -> IO (Either ProgramException (FilePath, FilePath))
parseMainArgs args = case args of
    []                           -> return $ Left $ InputException "No Arguments Provided"
    [something]                  -> return $ Left $ InputException "Only One Argument Provided"
    (inFile:outDir:something:_)  -> return $ Left $ InputException "Too Many Command Arguments"
    [inFile,outDir]  -> do
      fileExists      <- doesFileExist inFile
      directoryExists <- doesDirectoryExist outDir
      return $ case (fileExists, directoryExists) of
        (True,  True)  -> Right (inFile, outDir)
        (False, True)  -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist"
        (True,  False) -> Left $ InputException $ "Output Directory: " ++ outDir ++ " does not exist"
        (False, False) -> Left $ InputException $ "Input File: " ++ inFile ++ " does not exist"
                                                    ++ " and Output Directory: " ++ outDir ++ " does not exist"


logFilePath :: FilePath -> Log -> FilePath
logFilePath dir log = case log of
        UnknownLog _ _                  -> dir </> "unparsed_logs"  <.> ".log"
        KernelLog _ _ _ __ (Cpu (CpuNum num) _) -> dir </> T.unpack num  <.> ".log"
        _                              -> dir </> "parsed_logs"  <.> ".log"


groupL :: (Ord b) => [a] -> (a -> b) -> Map b [a]
groupL items f =
  let tups = map (\x -> (f x, [x])) items
  in  M.fromListWith (++) tups



sinkGroup :: (ToJSON b) => (a -> b) -> (FilePath, [a]) -> IO ()
sinkGroup f (fp,logs) =
  runConduitRes $
  CL.sourceList logs
  .| CL.map stringify
  .| B.sinkFile fp
  where stringify log = BS.snoc (toStrict $ encode $ toJSON $ f log) '\n'



main :: IO ()
main = do
    args        <- getArgs
    parsedArgs  <- parseMainArgs args
    case parsedArgs of
      Left e                 -> throw e
      Right (inFile, outDir) -> withFile inFile ReadMode (processFile outDir)
  where
    processFile dir inHandle = do
        content <- hGetContents inHandle
        let rows = lines content
            cLogs = map (parseLog . T.pack) rows
            grouped = M.toList $ groupL cLogs (logFilePath dir)
            sinks = map (sinkGroup toOutputLog) grouped
        sequence_ sinks


