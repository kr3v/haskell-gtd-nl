#if defined(mingw32_HOST_OS)
import Foreign.C
import System.IO
import Data.Functor ( void )
import Data.Int ( Int64 )
#else
import qualified GHC.Conc
#endif