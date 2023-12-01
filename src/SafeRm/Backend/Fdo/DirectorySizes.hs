{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for handling the directorysizes file in the
-- FDO spec.
module SafeRm.Backend.Fdo.DirectorySizes
  ( -- * Types
    DirectorySizes (..),
    DirectorySizesEntry (..),

    -- * Adding
    appendEntry,
    writeDirectorySizes,

    -- * Removing
    removeEntry,

    -- * Reading
    readDirectorySizes,
    readDirectorySizesTrashHome,
    getDirectorySizesPath,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.Foldable (Foldable (fold))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.FileReader qualified as FR
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathWriter qualified as PW
import GHC.Read (Read)
import SafeRm.Class.Serial
  ( Serial (DecodeExtra, decode, encode),
    decodeUnitThrowM,
    encodeThrowM,
  )
import SafeRm.Data.Paths (PathI, PathIndex (TrashHome))
import SafeRm.Env (HasTrashHome (getTrashHome))
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
import Text.Read qualified as TR

-- | directorysizes entry.
data DirectorySizesEntry = MkDirectorySizesEntry
  { -- | Directory size in bytes. This does __not__ include the size of the
    -- directory itself.
    size :: Natural,
    -- | The time this directory was deleted. The units are milliseconds since
    -- the unix epoch.
    time :: Natural,
    -- | Percent encoded filename.
    fileName :: ByteString
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''DirectorySizesEntry

instance Serial DirectorySizesEntry where
  type DecodeExtra DirectorySizesEntry = ()

  encode entry = Right $ mconcat bsXs
    where
      bsXs =
        [ toBs $ entry ^. #size,
          " ",
          toBs $ entry ^. #time,
          " ",
          entry ^. #fileName
        ]
      toBs = encodeUtf8 . T.pack . show

  decode _ bs = do
    (sizeBs, timeBs, fileNameBs) <- case C8.split ' ' bs of
      [sizeBs, timeBs, fileNameBs] -> Right (sizeBs, timeBs, fileNameBs)
      other -> Left $ "Expected three space-separated sections, received:" <> show other

    size <- fromBs sizeBs
    time <- fromBs timeBs

    Right
      $ MkDirectorySizesEntry
        { size,
          time,
          fileName = fileNameBs
        }
    where
      fromBs :: (Read a) => ByteString -> Either String a
      fromBs = TR.readEither <=< (bimap displayException T.unpack . decodeUtf8)

-- | Represents the directorysizes contents.
newtype DirectorySizes = MkDirectorySizes
  { unDirectorySizes :: Seq DirectorySizesEntry
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (Monoid, Semigroup) via (Seq DirectorySizesEntry)

makeFieldLabelsNoPrefix ''DirectorySizes

instance Serial DirectorySizes where
  type DecodeExtra DirectorySizes = ()

  encode (MkDirectorySizes dirSizes) =
    traverse encode dirSizes <&> fold . Seq.intersperse "\n"
  decode _ =
    fmap MkDirectorySizes
      . traverse (decode ())
      . Seq.fromList
      . C8.lines

-- | Appends an entry to directorysizes.
appendEntry ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTime m
  ) =>
  DirectorySizesEntry ->
  m ()
appendEntry dirSizeEntry = do
  MkDirectorySizes directorySizes <- readDirectorySizes
  let directorySizes' = MkDirectorySizes (directorySizes :|> dirSizeEntry)

  writeDirectorySizes directorySizes'

-- | Writes entries to directorysizes.
writeDirectorySizes ::
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTime m
  ) =>
  DirectorySizes ->
  m ()
writeDirectorySizes directorySizes = addNamespace "writeDirectorySizes" $ do
  directorySizesPath <- getDirectorySizesPath

  encoded <- encodeThrowM directorySizes

  tmpFile <- Utils.getRandomTmpFile [osp|directorysizes|]

  FW.writeBinaryFile tmpFile encoded

  PW.renameFile tmpFile directorySizesPath
    `catchAnyCS` \ex -> do
      $(logError) $ "Error renaming directorysizes: " <> displayExceptiont ex
      PW.removeFile tmpFile

-- | Removes an entry from directory sizes.
removeEntry ::
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadReader env m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTime m
  ) =>
  ByteString ->
  m ()
removeEntry entryName = do
  MkDirectorySizes decoded <- readDirectorySizes

  let directorySizes' = Seq.filter notEntry decoded

  writeDirectorySizes (MkDirectorySizes directorySizes')
  where
    notEntry = (/= entryName) . view #fileName

-- | Reads directorysizes.
readDirectorySizes ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m DirectorySizes
readDirectorySizes = asks getTrashHome >>= readDirectorySizesTrashHome

-- | Reads directorysizes.
readDirectorySizesTrashHome ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  m DirectorySizes
readDirectorySizesTrashHome path = do
  let directorySizesPath = trashHomeToDirectorySizes path
  exists <- doesFileExist directorySizesPath
  if exists
    then do
      bs <- FR.readBinaryFile directorySizesPath
      decodeUnitThrowM bs
    else pure mempty

getDirectorySizesPath ::
  ( HasTrashHome env,
    MonadReader env m
  ) =>
  m OsPath
getDirectorySizesPath = asks getTrashHome <&> trashHomeToDirectorySizes

trashHomeToDirectorySizes :: PathI TrashHome -> OsPath
trashHomeToDirectorySizes =
  (</> [osp|directorysizes|])
    . view #unPathI
