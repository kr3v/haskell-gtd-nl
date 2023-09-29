module Lib2 (module Lib2) where

lib2 :: IO Int
lib2 = return 1

type Lib2_ReexportedTypeAlias = String
