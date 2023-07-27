module Test1 (f11, Import1.f12, f21, ImportAs2.f22, ImportQ3.f31, f32, ImportQ4.f41, ImportQ4.f42, f43, f01) where

import Import1
import Import2 as ImportAs2
import qualified Import3 as ImportQ3
import qualified Import4 as ImportQ4 (f41)

f01 :: Int
f01 = 5
