module DataTypes where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

type URL = B.ByteString
data LinkResult = OK (URL,String) | LinkError (URL,String) | FormatError (URL,String) deriving (Show,Eq)
data LinkResult2 =
  LinkResult2 {referee :: URL,
              url :: URL,
              msg :: String}
  deriving (Show,Eq)
-- data LinkResult =
--   LinkResult {referee :: URL,
--               url :: URL,
--               msg :: String}
--   deriving (Show,Eq)
type Result = M.Map URL [(URL,LinkResult)]
