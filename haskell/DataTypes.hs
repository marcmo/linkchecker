module DataTypes where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

type URL = B.ByteString
data LinkResult = OK (URL,String) | LinkError (URL,String) | FormatError (URL,String) deriving (Show,Eq)
type Result = M.Map URL [(URL,LinkResult)]
