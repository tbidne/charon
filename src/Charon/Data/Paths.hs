{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for distinguishing path types.
module Charon.Data.Paths
  ( -- * Types
    PathI (..),
    PathIndex (..),

    -- ** Raw
    RawPathI (..),
    fromRaw,

    -- * Functions

    -- ** Specific
    containsTildePrefix,
    isEmpty,
    isRoot,
    isRoot',
    isDots,
    toString,
    toText,

    -- ** General
    -- $general
    showPaths,
    renderPath,
    renderPathQuote,
    reindex,
    (<//>),
    (<//),
    (//>),
    applyPathI,
    liftPathI,
    liftPathI',
    liftPathIF,
    liftPathIF',
    pathLength,
    textWidth,
  )
where

import Charon.Class.Serial (Serial (DecodeExtra, decode, encode))
import Charon.Prelude
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Effects.FileSystem.PathReader qualified as PR
import FileSystem.OsPath qualified as OsPath
import System.OsPath qualified as OsP
import Unicode.Grapheme qualified as Grapheme

-- | 'PathI' to 'String'. Attempts decoding for nicer display.
toString :: PathI i -> String
toString (MkPathI p) = decodeDisplayEx p

-- | 'PathI' to 'Text' via 'toString'.
toText :: PathI i -> Text
toText = T.pack . toString

-- TODO: We should really store absolute vs. relative here somehow.
-- It appears PathIndex refers to merely the "provenance", but doesn't
-- include abs vs. rel. E.g. We parse user paths as TrashEntryOriginalPath,
-- which could be rel or abs, then we use makeAbsolute before saving in
-- the trash. But this is not reflected anywhere in the type system.
--
-- Consider using Path over raw OsPath.

-- | Types of filepaths used in Charon.
data PathIndex
  = -- TRASH DIRECTORY PATHS

    -- | The trash directory.
    TrashHome
  | -- | The trash log file.
    TrashLog
  | -- | The directory to the trash files themselves i.e. <trash>/files.
    TrashDirFiles
  | -- | The directory to the trash info files i.e. <trash>/info.
    TrashDirInfo
  | -- TRASH ENTRY PATHS

    -- | The full trash path i.e. @\<trash-home\>\/files\/'\<trash-name\>@.
    TrashEntryPath
  | -- | The full trash info path i.e. @\<trash-home\>\/info\/'\<trash-name\>.trashinfo@.
    TrashEntryInfo
  | -- TRASH ENTRY FIELD TYPES

    -- | The name corresponding to some file/directory in the trash directory.
    TrashEntryFileName
  | -- | The original path for some file/directory in the trash directory.
    TrashEntryOriginalPath

-- | Indexed 'OsPath' so that we can prevent mixing up different filepaths.
type PathI :: PathIndex -> Type
newtype PathI (i :: PathIndex) = MkPathI
  { unPathI :: OsPath
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathI

instance Display (PathI i) where
  displayBuilder = displayBuilder . toText

-- | 'PathI' before any invariant checking e.g. tilde expansion.
type RawPathI :: PathIndex -> Type
newtype RawPathI (i :: PathIndex) = MkRawPathI
  { unRawPathI :: OsPath
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''RawPathI

fromRaw ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  RawPathI i ->
  m (PathI i)
fromRaw (MkRawPathI r) = MkPathI <$> PR.expandTilde r

-- TODO: If we have a total way to encode 'OsPath -> ByteString' then we can
-- make encode total, which will improve several type signatures. Technically
-- we could do this by utilizing OsPath's underlying ShortByteString, but
-- that requires digging into the internals.
--
-- There is an open issue for a Binary interface: once that lands, we can use
-- the same serialization strategy.
--
-- https://github.com/haskell/filepath/issues/161

instance Serial (PathI i) where
  type DecodeExtra (PathI i) = ()

  encode =
    bimap displayException (encodeUtf8 . T.pack)
      . OsPath.decode
      . view #unPathI

  decode _ bs = case TEnc.decodeUtf8' bs of
    Left ex -> Left $ displayException ex
    Right t ->
      bimap displayException MkPathI
        . encodeValid
        . T.unpack
        $ t

-- | Modifies the index.
reindex :: PathI i -> PathI j
reindex = liftPathI id

-- | Lifts an 'OsPath' transformation to 'PathI', allowing for the index to
-- change.
liftPathI :: (OsPath -> OsPath) -> PathI i -> PathI j
liftPathI f (MkPathI fp) = MkPathI (f fp)

-- | 'liftPathI' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
liftPathI' :: (OsPath -> OsPath) -> PathI i -> PathI i
liftPathI' = liftPathI

-- | Lifts an effectful 'FilePath' transformation to 'PathI'.
liftPathIF ::
  (Functor f, HasCallStack) =>
  ((HasCallStack) => OsPath -> f OsPath) ->
  PathI i ->
  f (PathI j)
liftPathIF f = fmap MkPathI . applyPathI f

-- | 'liftPathIF' specialized to the same index. This should be preferred
-- as the former is easier to use incorrectly.
liftPathIF' ::
  (Functor f) =>
  ((HasCallStack) => OsPath -> f OsPath) ->
  PathI i ->
  f (PathI i)
liftPathIF' = liftPathIF

-- | Lifts an 'OsPath' function to 'PathI'.
applyPathI :: (HasCallStack) => ((HasCallStack) => OsPath -> a) -> PathI i -> a
applyPathI f = f . view #unPathI

-- | '(</>)' lifted to 'PathI'. Notice the index can change, so take care.
(<//>) :: PathI i1 -> PathI i2 -> PathI i3
MkPathI x <//> MkPathI y = MkPathI (x </> y)

infixr 5 <//>

(<//) :: PathI i1 -> OsPath -> PathI i2
MkPathI x <// y = MkPathI (x </> y)

infixl 5 <//

(//>) :: OsPath -> PathI i1 -> PathI i2
(//>) = flip (<//)

infixl 5 //>

-- | Returns true if the path contains a tilde (~).
containsTildePrefix :: PathI i -> Bool
containsTildePrefix = OsPath.containsTildePrefix . view #unPathI

--- | Returns true if the paths is empty. Note that whitespace is __not__
--- considered empty as we are trying to prevent deleting "" (which gets
--- turned into the current working directory). But posix filepaths can be
--- whitespace (e.g. " "), and that is fine.
isEmpty :: (MonadThrow m) => PathI i -> m Bool
isEmpty (MkPathI p) = do
  s <- OsPath.decodeThrowM p
  -- NOTE: This is NOT redundant as the typical OsPath creation functions
  -- e.g. encodeUtf do NOT check for empty (or other invariants). We need to
  -- use the 'isValid' check instead.
  --
  -- Our optparse-applicative ReadM function osPath does in fact check this,
  -- so hopefully all of our OsPaths are valid. Nevertheless we leave this
  -- check in to be safe.
  pure $ null s

-- | Returns true if the path is the root.
isRoot :: (MonadThrow m) => PathI i -> m Bool
isRoot = isRoot' . view #unPathI

-- | Returns true if the path is the root.
isRoot' :: (MonadThrow m) => OsPath -> m Bool
#if WINDOWS
isRoot' p = do
  fp <- OsPath.decodeThrowM p
  pure $ f . T.unpack . T.strip . T.pack $ fp
  where
    f (_ : ':' : rest) = null rest || rest == "\\" || rest == "\\\\"
    f _ = False
#else
isRoot' p = do
  fp <- OsPath.decodeThrowM p
  pure $ (== "/") . T.strip . T.pack $ fp
#endif

-- | Returns true if the path __ends__ in nothing but dots e.g.
-- @/path/to/dots/..@.
isDots :: (MonadThrow m) => PathI i -> m Bool
isDots (MkPathI p) = do
  let p' = OsP.takeFileName p
  fp <- OsPath.decodeThrowM p'
  pure $ isDots' . T.unpack . T.strip . T.pack $ fp
  where
    isDots' fp = L.all (== '.') fp && not (null fp)

-- | Pretty-print a list of paths.
--
-- >>> showPaths ["one", "two"]
-- "one, two"
showPaths :: [PathI a] -> String
showPaths = L.intercalate ", " . fmap (show . view #unPathI)

renderPath :: PathI i -> Text
renderPath = T.pack . decodeLenient . view #unPathI

renderPathQuote :: PathI i -> Text
renderPathQuote = (\s -> "'" <> s <> "'") . renderPath

pathLength :: PathI i -> Int
pathLength (MkPathI p) =
  textWidth
    . T.pack
    . OsPath.decodeLenient
    $ p

textWidth :: Text -> Int
textWidth = Grapheme.textWidth

-- $general
-- These functions allows for lifting arbitrary 'OsPath' functions onto our
-- 'PathI'. Note that this can easily invalidate any invariants we would
-- like to hold (e.g. appending a path can turn a directory into a file),
-- so caution must be exercised.
