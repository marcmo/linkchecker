module DataTypes
        ( URL
        , LinkResult(..)
        , Result
        ) where


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as B

type URL = B.ByteString
data LinkResult = OK (URL,String) | LinkError (URL,String) | FormatError (URL,String) deriving (Show,Eq)
-- data LinkResult2 =
--   LinkResult2 { referee :: URL,
--                 url :: URL,
--                 msg :: String}
--   deriving (Show,Eq)
type Result = M.Map URL (String, S.Set URL)
