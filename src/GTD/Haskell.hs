{-# LANGUAGE LambdaCase #-}

module GTD.Haskell where

import Control.Monad (forM_)
import Data.ByteString.UTF8 (fromString)
import Language.Haskell.Exts (Decl (..), Module (..), ParseMode (..), ParseResult (..), SrcSpanInfo, defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

haskellApplyCppHs :: FilePath -> String -> IO String
haskellApplyCppHs = runCpphs defaultCpphsOptions

haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

haskellGetIdentifiers :: Module SrcSpanInfo -> IO ()
haskellGetIdentifiers p = do
  let (Module src head wtf1 imports wtf2) = p
  forM_ wtf2 $ \case
    TypeSig src ident _ -> print ident
    _ -> return ()
  print "<<<<***>>>>\n"
