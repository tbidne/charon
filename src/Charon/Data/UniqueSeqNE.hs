{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeqNE' type.
module Charon.Data.UniqueSeqNE
  ( UniqueSeqNE (MkUniqueSeqNE),

    -- * Creation
    Internal.singleton,
    Internal.fromFoldable1,

    -- * Lookup
    Internal.member,

    -- * Operations
    Internal.prepend,
    Internal.append,
    Internal.union,
    Internal.map,

    -- * UniqueSet
    Internal.unsafefromUniqueSeq,
    Internal.toUniqueSeq,
  )
where

import Charon.Data.UniqueSeqNE.Internal (UniqueSeqNE (MkUniqueSeqNE))
import Charon.Data.UniqueSeqNE.Internal qualified as Internal
