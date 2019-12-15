{-# LANGUAGE DeriveGeneric #-}
module Write where
import GHC.Generics
import Data.Aeson
import Data.Text
import Prelude hiding (unwords)
import Lib

data OutputLog = OutputCpuLog { date      :: !Text
                              , machine   :: !Text
                              , eventTime :: !Double
                              , cpu :: !Text
                              , state :: CpuState }
                | OutputParsedLog { date      :: !Text
                                  , machine   :: !Text
                                  , eventTime :: !Double
                                  , device    :: !Text 
                                  , message     :: !Text } 
                | UnknownParsedLog { logMessage :: !Text
                                   , err :: !String } 
                deriving (Show, Generic)



instance ToJSON CpuState
instance ToJSON OutputLog

toOutputLog :: KernelLog -> OutputLog
toOutputLog (KernelLog (DateFormat mo day ts) (Machine b) (EventTime c) _ (Cpu (CpuNum num) state)) = 
  OutputCpuLog { date = unwords [mo,day,ts]
               , machine = b
               , eventTime = c
               , cpu = num
               , state = state}
toOutputLog (KernelLog (DateFormat mo day ts) (Machine b) (EventTime c) (Device d) (Event (GenericMessage mess))) = 
  OutputParsedLog { date = unwords [mo,day,ts]
                  , machine = b
                  , eventTime = c
                  , device = d
                  , message = mess}
toOutputLog (UnknownLog mess err) = 
  UnknownParsedLog { logMessage = mess
                   , err = err}

