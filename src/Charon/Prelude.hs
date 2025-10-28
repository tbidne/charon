{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Custom prelude.
module Charon.Prelude
  ( module X,

    -- * Text
    bsToStr,
    bsToStrLenient,
    showt,
    displayExceptiont,
    decodeDisplayExT,

    -- * Display Utils
    (<+>),
    vsep,
    punctuate,
    line,

    -- * Debug
    todo,

    -- * Optics
    packed,
    unpacked,

    -- ** Path literals
    pathDotTrash,
    pathCharon,

    -- * Misc
    charonPath,
    doesAnyPathExist,
    doesAnyPathNotExist,
    throwLeft,
    usingReaderT,
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (liftA2, pure, (<*>)),
    (*>),
  )
import Control.Category as X (Category ((.)), (>>>))
import Control.DeepSeq as X (NFData)
import Control.Exception as X (IOException)
import Control.Exception.Utils as X
  ( StringException (MkStringException),
    catchSync,
    catchesSync,
    exitFailure,
    throwString,
    throwText,
    trySync,
  )
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
import Control.Monad.Catch as X
  ( Exception (displayException),
    Handler (Handler),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    bracket,
    catch,
    catches,
    finally,
    throwM,
    try,
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadReader (ask),
    ReaderT,
    asks,
    local,
    runReaderT,
  )
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X (Bytes (MkBytes), Size (B), _MkBytes)
import Data.Char as X (Char)
import Data.Coerce as X (coerce)
import Data.Either as X (Either (Left, Right), either)
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X
  ( Foldable (fold, foldMap', foldl', foldr, foldr1, length),
    foldlM,
    for_,
    null,
    sequenceA_,
    traverse_,
  )
import Data.Foldable1 as X
  ( Foldable1 (foldMap1),
    foldlM1,
    foldlMapM1,
  )
import Data.Function as X (const, flip, id, ($))
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
import Data.Text.Builder.Linear (Builder)
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Traversable as X (for, traverse)
import Data.Tuple as X (curry, fst, uncurry)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality as X (type (~))
#endif
import Data.Vector as X (Vector)
import Data.Word as X (Word16, Word8)
import Effects.Concurrent.Async as X (MonadAsync)
import Effects.Concurrent.Thread as X (MonadThread)
import Effects.FileSystem.FileReader as X
  ( MonadFileReader (readBinaryFile),
    decodeUtf8,
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
        getTemporaryDirectory,
        listDirectory,
        makeAbsolute
      ),
    PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
    doesSymbolicLinkExist,
    getXdgConfig,
  )
import Effects.FileSystem.PathWriter as X
  ( MonadPathWriter
      ( createDirectoryIfMissing,
        removeDirectoryRecursive,
        removePathForcibly,
        renameDirectory,
        renameFile
      ),
  )
import Effects.Haskeline as X (MonadHaskeline, getInputChar, getInputLine)
import Effects.IORef as X
  ( IORef,
    MonadIORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Effects.Logger as X
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    MonadLogger (monadLoggerLog),
    levelFatal,
    logDebug,
    logError,
    logFatal,
    logInfo,
    logWarn,
  )
import Effects.Logger.Namespace as X
  ( HasNamespace,
    MonadLoggerNS,
    Namespace,
    addNamespace,
  )
import Effects.Optparse as X (MonadOptparse (execParser))
#if !WINDOWS
import Effects.System.Posix.Files as X (MonadPosixFiles)
#endif
import Effects.System.PosixCompat.Files as X (MonadPosixCompatFiles)
import Effects.System.Terminal as X
  ( MonadTerminal (putStr, putStrLn),
    print,
    putTextLn,
  )
import Effects.Time as X (MonadTime)
import FileSystem.OsPath as X
  ( OsPath,
    decodeDisplayEx,
    decodeLenient,
    encodeValid,
    encodeValidThrowM,
    osp,
    ospPathSep,
    (<.>),
    (</>),
  )
import FileSystem.OsString as X
  ( OsString,
    osstr,
  )
import GHC.Enum as X (Bounded (maxBound, minBound), Enum (toEnum))
import GHC.Err as X (error, undefined)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Real as X (Integral (div), even, fromIntegral, realToFrac)
import GHC.Stack as X
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
  )
import Numeric.Convert.Integer as X (FromInteger, fromℤ, toℤ)
import Numeric.Convert.Rational as X (FromRational, fromℚ)
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
    lensVL,
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
import Optics.Core.Extras as X (is)
import Optics.TH as X
  ( generateUpdateableOptics,
    makeFieldLabelsNoPrefix,
    makeFieldLabelsWith,
    makePrisms,
    noPrefixFieldLabels,
  )
import PathSize as X (findLargestPaths)
import PathSize.Utils as X (MonadPosixFilesC)
import System.Exit as X (ExitCode (ExitFailure, ExitSuccess))
import System.IO as X
  ( FilePath,
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

usingReaderT :: b -> ReaderT b m a -> m a
usingReaderT = flip runReaderT

pathCharon :: OsPath
pathCharon = [osp|charon|]

pathDotTrash :: OsPath
pathDotTrash = [osp|.trash|]

doesAnyPathExist ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m Bool
doesAnyPathExist p = do
  symlinkExists <- doesSymbolicLinkExist p
  if symlinkExists
    then pure True
    else doesPathExist p

doesAnyPathNotExist ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m Bool
doesAnyPathNotExist = fmap not . doesAnyPathExist

decodeDisplayExT :: OsPath -> Text
decodeDisplayExT = T.pack . decodeDisplayEx

vsep :: [Builder] -> Builder
vsep = concatWith (\x y -> x <> line <> y)

punctuate :: Builder -> [Builder] -> [Builder]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate p (x : xs) = x <> p : punctuate p xs

(<+>) :: Builder -> Builder -> Builder
x <+> y = x <> " " <> y

line :: Builder
line = "\n"

-- vendored from prettyprinter, for text's Builder
concatWith :: (Foldable t) => (Builder -> Builder -> Builder) -> t Builder -> Builder
concatWith f ds
  | null ds = mempty
  | otherwise = foldr1 f ds

-- | Placeholder for unwritten code.
todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING todo "todo remains in code" #-}

charonPath :: OsPath
charonPath = [osp|charon|]

throwLeft :: forall m e a. (Exception e, HasCallStack, MonadThrow m) => Either e a -> m a
throwLeft (Right x) = pure x
throwLeft (Left e) = throwM e
