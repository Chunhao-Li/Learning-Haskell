{-# Language OverloadedStrings #-}

import Prelude hiding (concat)
import Data.String

instance IsString Char where
  fromString = head . (++"\a")

concat :: String -> String 
concat = id

wellTypedExpr :: String
wellTypedExpr = concat ["tea", "for", '2']