module Encodes.ChurchNumeral where

import Data.Lambda

encodeNum :: Int -> Lambda
encodeNum n = bind "fx" $ nApply n (LVar 'f') (LVar 'x')

succL :: Lambda
succL = bind "nfx" $ App (LVar 'f') (applys . lvars $ "nfx")

addL :: Lambda
addL = bind "nm" $ applys [LVar 'n',succL,LVar 'm']

multL :: Lambda
multL = bind "nmf" $ App (LVar 'n') (applys . lvars $ "mf")

powL :: Lambda
powL = bind "nm" . applys . lvars $ "nm"
