{-# LANGUAGE TypeApplications #-}

#ifdef WINDOWS
module System.OsPath.Windows
#elif defined(POSIX)
module System.OsPath.Posix
#else
module System.OsPath
#endif
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
  , WindowsPath
#elif defined(POSIX)
    PosixString
  , PosixChar
  , PosixPath
#else
    OsPath
  , OsString
  , OsChar
#endif
  -- * Filepath construction
  , PS.encodeUtf
  , PS.encodeWith
  , PS.encodeFS
#if defined(WINDOWS) || defined(POSIX)
  , pstr
#else
  , osp
#endif
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


#ifdef WINDOWS
import System.OsPath.Types
import System.OsString.Windows as PS
    ( unsafeFromChar
    , toChar
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import Data.Bifunctor ( bimap )
import qualified System.OsPath.Windows.Internal as C
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.Monad ( when )

#elif defined(POSIX)
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )
import Control.Monad ( when )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )

import GHC.IO.Encoding.UTF8 ( mkUTF8 )
import System.OsPath.Types
import System.OsString.Posix as PS
    ( unsafeFromChar
    , toChar
    , decodeUtf
    , decodeWith
    , decodeFS
    , pack
    , encodeUtf
    , encodeWith
    , encodeFS
    , unpack
    )
import Data.Bifunctor ( bimap )
import qualified System.OsPath.Posix.Internal as C

#else

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

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.OsPath.Windows as C
#else
import qualified System.OsPath.Posix as C
#endif

import Data.Bifunctor
    ( bimap )
#endif
import System.OsString.Internal.Types


#ifdef WINDOWS_DOC
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > pathSeparator == '\\'S
#elif defined(POSIX_DOC)
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > pathSeparator ==  '/'
#else
-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'S
-- > Posix:   pathSeparator ==  '/'
#endif
pathSeparator :: WORD_NAME
pathSeparator = WORD_NAME C.pathSeparator
