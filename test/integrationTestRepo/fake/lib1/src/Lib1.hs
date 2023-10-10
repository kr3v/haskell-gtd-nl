module Lib1 (lib1, lib2, module Lib2) where

import Lib2
import Lib2 (lib2)

lib1 :: Int
lib1 = 1

data Lib1_Data
  = Lib1_Data1
      { lib1_data1_1 :: Int,
        lib1_data1_2 :: Int
      }
  | Lib1_Data2
      { lib1_data2_1 :: Int
      }
  | Lib1_Data1_Dup
      { lib1_data1_1 :: Int
      }