{-# LANGUAGE OverloadedStrings #-}

module Encodes.ChurchNumeral where

import Data.Lambda

cn :: Int -> Lambda
cn n = bind "f" $ bind "x" $ nApply n "f" "x"

succL :: Lambda
succL = bind "n" $ bind "f" $ bind "x" $ "f" <@> ("n" <@> "f" <@> "x")

-- addL :: Lambda
-- addL = binds "nm" $ "n" <@> succL <@> "m"
-- 
-- multL :: Lambda
-- multL = binds "nmf" $ "n" <@> "m" <@> "f"
-- 
-- powL :: Lambda
-- powL = binds "mn" $ "n" <@> "m"
