{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}



module Main where
import           Safe (atMay)
import           Control.Exception
import           Control.Monad
import           Data.Maybe
import           Lib
import           Write
import           System.Environment
import           System.IO
import           Data.Typeable
import           System.Directory
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as BSO
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (toJSON, encode)
import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Binary as B
import qualified Data.Conduit.List as CL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import           System.FilePath
import qualified Data.Foldable as F

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


logFilePath :: FilePath -> KernelLog -> FilePath
logFilePath dir log = case log of
        UnknownLog _ _                  -> dir </> "unparsed_logs"  <.> ".log"
        KernelLog _ _ _ __ (Cpu (CpuNum num) _) -> dir </> T.unpack num  <.> ".log"
        _                              -> dir </> "parsed_logs"  <.> ".log"


groupL :: (Ord b) => [a] -> (a -> b) -> M.Map b [a]
groupL items f = 
  let tups = map (\x -> (f x, [x])) items
  in  M.fromListWith (++) tups



sinkGroup :: (FilePath, [KernelLog]) -> IO ()
sinkGroup (fp,logs) =  
  runConduitRes $ 
  CL.sourceList logs 
  .| CL.map stringify 
  .| B.sinkFile fp
  where stringify log = BS.snoc (toStrict $ encode $ toJSON $ toOutputLog log) '\n'



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
            sinks = map sinkGroup grouped
        sequence_ sinks




groupChunk :: FilePath -> KernelLog -> KernelLog -> Bool
groupChunk dir l1 l2 = wPath l1 == wPath l2
  where wPath = logFilePath dir


