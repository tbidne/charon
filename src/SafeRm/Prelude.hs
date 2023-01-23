{-# LANGUAGE CPP #-}

-- | Custom prelude.
--
-- @since 0.1
module SafeRm.Prelude
  ( module X,

    -- * Text
    showt,
    displayExceptiont,
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (pure, (<*>)),
    (*>),
  )
import Control.DeepSeq as X (NFData)
import Control.Monad as X
  ( Monad ((>>=)),
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Logger as X
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    MonadLogger (monadLoggerLog),
    logDebug,
    logError,
    logInfo,
    logWarn,
  )
import Control.Monad.Reader as X
  ( MonadReader (ask),
    ReaderT,
    asks,
    local,
    runReaderT,
  )
import Data.Bifunctor as X (Bifunctor (bimap))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X (Bytes (MkBytes), Size (B))
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right), either)
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (foldMap', foldl', foldr, length),
    for_,
    null,
    sequenceA_,
  )
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (fmap), ($>), (<$>), (<&>))
import Data.HashMap.Strict as X (HashMap)
import Data.HashSet as X (HashSet)
import Data.Hashable as X (Hashable (hashWithSalt))
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter, zipWith)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X
  ( Ord (compare, (<), (<=), (>), (>=)),
    Ordering (EQ, GT, LT),
    min,
  )
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq, (<|), (|>))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Traversable as X (traverse)
import Data.Tuple as X (curry, uncurry)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality as X (type (~))
#endif
import Data.Vector as X (Vector)
import Data.Word as X (Word16, Word8)
import Effects.Exception as X
  ( AnnotatedException (..),
    Exception (..),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    addCallStack,
    bracket,
    catch,
    catchAny,
    catchWithCallStack,
    displayCallStack,
    displayNoCallStack,
    finally,
    throwM,
    throwString,
    throwWithCallStack,
    try,
    tryWithCallStack,
  )
import Effects.FileSystem.FileReader as X
  ( MonadFileReader (readBinaryFile),
    decodeUtf8Lenient,
    readFileUtf8ThrowM,
  )
import Effects.FileSystem.FileWriter as X
  ( MonadFileWriter (appendBinaryFile, writeBinaryFile),
    encodeUtf8,
  )
import Effects.FileSystem.HandleWriter as X
  ( MonadHandleWriter
      ( hClose,
        hFlush,
        hPut,
        openBinaryFile
      ),
  )
import Effects.FileSystem.PathReader as X
  ( MonadPathReader
      ( canonicalizePath,
        doesDirectoryExist,
        doesFileExist,
        doesPathExist,
        getFileSize,
        getHomeDirectory,
        listDirectory
      ),
    getXdgConfig,
  )
import Effects.FileSystem.PathSize as X (MonadPathSize (findLargestPaths))
import Effects.FileSystem.PathWriter as X
  ( MonadPathWriter
      ( createDirectoryIfMissing,
        removeDirectoryRecursive,
        removePathForcibly,
        renameDirectory,
        renameFile
      ),
  )
import Effects.IORef as X
  ( IORef,
    MonadIORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Effects.LoggerNamespace as X
  ( MonadLoggerNamespace (getNamespace, localNamespace),
    addNamespace,
  )
import Effects.Optparse as X (MonadOptparse (execParser))
import Effects.System.Exit as X (ExitCode (..), MonadExit, exitFailure)
import Effects.System.Terminal as X
  ( MonadTerminal (putStr, putStrLn),
    putTextLn,
  )
import Effects.Time as X (MonadTime)
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (toEnum))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Real as X (even, fromIntegral)
import GHC.Stack as X
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
  )
import Optics.Core as X
  ( A_Getter,
    A_Lens,
    A_Setter,
    AffineTraversal',
    Is,
    Iso',
    LabelOptic (labelOptic),
    LabelOptic',
    Lens,
    Lens',
    Optic',
    Prism',
    iso,
    lens,
    over',
    preview,
    prism,
    review,
    set',
    view,
    (%),
    (%?),
    (^.),
    (^?),
    _1,
    _2,
    _3,
    _4,
    _Just,
  )
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrisms)
import Prettyprinter as X
  ( Doc,
    Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text as X (renderStrict)
import System.FilePath as X ((</>))
import System.IO as X
  ( BufferMode (NoBuffering),
    FilePath,
    Handle,
    IO,
    IOMode (AppendMode),
  )
import Text.Show as X (Show (show))

-- | @since 0.1
showt :: Show a => a -> Text
showt = T.pack . show

-- | @since 0.1
displayExceptiont :: Exception e => e -> Text
displayExceptiont = T.pack . displayException
