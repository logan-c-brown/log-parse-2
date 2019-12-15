{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where
import Control.Applicative
import GHC.Generics
import Data.Text
import Data.Attoparsec.Text as P
import Data.Char (isSpace, isDigit)



data KernelLog = KernelLog DateFormat Machine EventTime Device Event 
               | UnknownLog Text String 
               deriving (Show)

data CpuState = Online
              | Normal
              | AboveNormal
              | Offline
              | Unknown
              deriving (Show, Generic)

newtype CpuNum = CpuNum Text deriving (Show)

data Event = Event GenericMessage
           | Cpu CpuNum CpuState 
           deriving (Show)

data DateFormat = DateFormat Text Text Text  deriving (Show)
newtype GenericMessage = GenericMessage Text deriving (Show)
newtype Machine = Machine Text  deriving (Show)
newtype EventTime = EventTime Double deriving (Show)
newtype Device = Device Text deriving (Show)




dateP :: Parser DateFormat
dateP = DateFormat <$> P.take 3
                   <* skipSpace
                   <*> P.takeWhile isDigit
                   <* skipSpace
                   <*> (intercalate ":" <$> P.take 2 `sepBy` char ':')


machineP :: Parser Machine
machineP = Machine <$> P.takeWhile1 (not . isSpace)


timeP :: Parser EventTime
timeP = do string "kernel: [" >> skipSpace
           num <- double
           string "]"
           return $ EventTime num

deviceP :: Parser Device
deviceP = Device <$> ((P.takeWhile1 (/= ':') <* string ":" )
                 <|> string "")


cpuNumP :: Parser CpuNum
cpuNumP = do string "CPU"
             string " " <|> string ""
             num <- P.takeWhile1 isDigit
             pure $ CpuNum num


cpuStateP :: Parser CpuState
cpuStateP = 
        (string "Package temperature/speed normal" <* takeText >> return Normal )
    <|> (string "Core temperature/speed normal" <* takeText    >> return Normal )
    <|> (string "Core temperature above threshold" <* takeText >> return AboveNormal )
    <|> (string "Package temperature above threshold" <* takeText >> return AboveNormal )
    <|> (string "is up"                                        >> return Online )
    <|> (string "is now offline"                               >> return Offline )
    <|> (takeText                                              >> return Unknown )


cpuP :: Parser Event
cpuP = do num <- cpuNumP
          br <- string ": " <|> string " "
          st  <- cpuStateP
          pure $ Cpu num st


eventP :: Parser Event
eventP = cpuP <* takeText 
     <|> Event . GenericMessage <$> takeText

     --Dec 14 14:23:40 x1e kernel: [ 9956.910816] mce: CPU3: Package temperature/speed normal

kernelLogP :: Parser KernelLog
kernelLogP = do date <- dateP
                skipSpace
                machine <- machineP
                skipSpace
                time <- timeP
                skipSpace
                device <- deviceP
                skipSpace
                event <- eventP
                pure $ KernelLog date machine time device event


parseLog :: Text -> KernelLog
parseLog s = case parseOnly kernelLogP s of
               Left err -> UnknownLog s err
               Right parsedLog -> parsedLog

