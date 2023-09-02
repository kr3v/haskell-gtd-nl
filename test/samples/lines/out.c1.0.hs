#line 1 "in.0.hs"
{-# LANGUAGE CPP #-}





#line 1 "././test/samples/lines/in.0.header.hs"
{-# LANGUAGE TypeApplications #-}






module System.OsPath

  (
  -- * Types









    OsPath
  , OsString
  , OsChar

  -- * Filepath construction
  , PS.encodeUtf
  , PS.encodeWith
  , PS.encodeFS



  , osp

  , PS.pack

  -- * Filepath deconstruction
  , PS.decodeUtf
  , PS.decodeWith
  , PS.decodeFS
  , PS.unpack

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar

  -- * Separator predicates
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator

  -- * $PATH methods
  , splitSearchPath,

  -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
  )
where





















































import System.OsPath.Internal as PS
    ( osp
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import System.OsPath.Types
    ( OsPath )
import System.OsString ( unsafeFromChar, toChar )




import qualified System.OsPath.Posix as C


import Data.Bifunctor
    ( bimap )

import System.OsString.Internal.Types













-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'S
-- > Posix:   pathSeparator ==  '/'

pathSeparator :: OsChar
pathSeparator = OsChar C.pathSeparator

#line 8 "in.0.hs"

palka :: Kopalka
palka = yam stick
