{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Custom prelude.
module SafeRm.Prelude
  ( module X,

    -- * Text
    bsToStr,
    bsToStrLenient,
    showt,
    displayExceptiont,

    -- * Optics
    packed,
    unpacked,

    -- ** Path literals
    pathDotTrash,
    pathFiles,
    pathInfo,
    pathSafeRm,
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (liftA2, pure, (<*>)),
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
import Data.Bifunctor as X (Bifunctor (bimap))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X (Bytes (MkBytes), Size (B), _MkBytes)
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
import Data.List as X (filter, zipWith, (++))
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
import Data.Sequence as X (Seq ((:<|), (:|>)))
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)))
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
import Effectful as X (Eff, IOE, runEff, type (:>))
import Effectful.Concurrent as X (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic as X (interpret, localSeqUnlift, reinterpret)
import Effectful.Exception as X
  ( Exception (displayException),
    ExitCode (ExitFailure, ExitSuccess),
    MonadThrow,
    SomeException,
    bracket,
    catch,
    catchAny,
    exitFailure,
    finally,
    throwM,
    throwString,
    try,
    tryAny,
  )
import Effectful.FileSystem.FileReader.Dynamic as X
  ( FileReaderDynamic,
    readBinaryFile,
    readFileUtf8ThrowM,
  )
import Effectful.FileSystem.FileWriter.Dynamic as X
  ( FileWriterDynamic,
    appendBinaryFile,
    writeBinaryFile,
  )
import Effectful.FileSystem.HandleWriter.Dynamic as X
  ( HandleWriterDynamic,
    hClose,
    hFlush,
    hPut,
    openBinaryFile,
  )
import Effectful.FileSystem.PathReader.Dynamic as X
  ( PathReaderDynamic,
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getFileSize,
    getHomeDirectory,
    getXdgConfig,
    getXdgState,
    listDirectory,
    runPathReaderDynamicIO,
  )
import Effectful.FileSystem.PathWriter.Dynamic as X
  ( PathWriterDynamic,
    createDirectoryIfMissing,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
    renameDirectory,
    renameFile,
    runPathWriterDynamicIO,
  )
import Effectful.FileSystem.Utils as X
  ( OsPath,
    decodeOsToFp,
    decodeOsToFpShow,
    decodeOsToFpShowText,
    decodeOsToFpThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    encodeFpToOs,
    encodeFpToOsThrowM,
    encodeUtf8,
    osp,
    (</>),
  )
import Effectful.IORef.Static as X
  ( IORef,
    IORefStatic,
    modifyIORef',
    newIORef,
    readIORef,
    runIORefStaticIO,
    writeIORef,
  )
import Effectful.Logger.Dynamic as X
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    LoggerDynamic,
    logDebug,
    logError,
    logInfo,
    logWarn,
    loggerLog,
  )
import Effectful.LoggerNS.Dynamic as X
  ( LoggerNSDynamic,
    Namespace,
    addNamespace,
    getNamespace,
    localNamespace,
  )
import Effectful.Optparse.Static as X (OptparseStatic, runOptparseStaticIO)
import Effectful.PosixCompat.Static as X
  ( PosixCompatStatic,
    runPosixCompatStaticIO,
  )
import Effectful.Reader.Static as X
  ( Reader,
    ask,
    asks,
    local,
    runReader,
  )
import Effectful.Terminal.Dynamic as X
  ( TerminalDynamic,
    print,
    putStr,
    putStrLn,
    putTextLn,
  )
import Effectful.Time.Dynamic as X (TimeDynamic)
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
    Getter,
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
    to,
    view,
    (%),
    (%?),
    (&),
    (.~),
    (^.),
    (^?),
    _1,
    _2,
    _3,
    _4,
    _Just,
  )
import Optics.TH as X
  ( generateUpdateableOptics,
    makeFieldLabelsNoPrefix,
    makeFieldLabelsWith,
    makePrisms,
    noPrefixFieldLabels,
  )
import PathSize as X (findLargestPaths)
import Prettyprinter as X
  ( Doc,
    Pretty (pretty),
    layoutCompact,
    line,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Text as X (renderStrict)
import System.IO as X
  ( BufferMode (NoBuffering),
    FilePath,
    Handle,
    IO,
    IOMode (AppendMode),
  )
import Text.Show as X (Show (show))

showt :: (Show a) => a -> Text
showt = T.pack . show

displayExceptiont :: (Exception e) => e -> Text
displayExceptiont = T.pack . displayException

bsToStr :: ByteString -> String
bsToStr = either displayException T.unpack . decodeUtf8

bsToStrLenient :: ByteString -> String
bsToStrLenient = T.unpack . decodeUtf8Lenient

-- Vendoring optics-extra Data.Text.Strict.Optics

packed :: Iso' String Text
packed = iso T.pack T.unpack
{-# INLINE packed #-}

unpacked :: Iso' Text String
unpacked = iso T.unpack T.pack
{-# INLINE unpacked #-}

pathFiles :: OsPath
pathFiles = [osp|files|]

pathInfo :: OsPath
pathInfo = [osp|info|]

pathSafeRm :: OsPath
pathSafeRm = [osp|safe-rm|]

pathDotTrash :: OsPath
pathDotTrash = [osp|.trash|]
