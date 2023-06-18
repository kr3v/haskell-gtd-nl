{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Writer.Lazy
import Data.Aeson (Value, genericToJSON)
import Data.Aeson.TH (defaultOptions)
import Data.Data (Data (gfoldl, toConstr), showConstr)
import Data.Foldable (forM_)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, SrcSpanAnn' (..))
import GHC.Parser (parseModule)
import GHC.Parser.Lexer (P (unP), ParseResult (..), ParserOpts, initParserState, mkParserOpts)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), mkRealSrcLoc)
import GHC.Utils.Error (DiagOpts (..))
import GHC.Utils.Outputable (Outputable (..), defaultSDocContext, renderWithContext)
import Language.Haskell.Syntax
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Extension (XRec (..))
import System.Environment (getArgs)
import Text.Printf (printf)

runParser :: FilePath -> ParserOpts -> String -> P a -> ParseResult a
runParser filename opts str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext . ppr

showS :: Outputable a => SrcSpanAnn' a -> String
showS (SrcSpanAnn ann loc) = showO ann ++ " " ++ showO loc

data Definition = Definition
  { definitionIdentifier :: FilePath,
    definitionLocation :: SrcSpan
  }
  deriving (Show, Eq)

haskellGhcCollectDefinitions :: FilePath -> HsModule GhcPs -> WriterT [Definition] IO ()
haskellGhcCollectDefinitions src m = do
  forM_ (hsmodDecls m) \d -> do
    let (L a b) = d
    case b of
      SigD e f -> do
        case f of
          TypeSig g h i -> do
            forM_ h \(L (SrcSpanAnn ann loc) k) -> do
              tell [Definition (showO k) loc]
          _ -> return ()
      _ -> return ()

main :: IO ()
main = do
  let src = "./app/Main.hs"
  content <- readFile src

  let diagOpts = DiagOpts EnumSet.empty EnumSet.empty False False Nothing defaultSDocContext
  let opts = mkParserOpts EnumSet.empty diagOpts [] False False False False
  let r = runParser src opts content parseModule
  case r of
    POk _ (L l e) -> do
      print $ (showConstr . toConstr) e
      decls <- execWriterT (haskellGhcCollectDefinitions src e)
      forM_ decls \decl -> do print decl
    PFailed _ -> putStrLn "Failed"