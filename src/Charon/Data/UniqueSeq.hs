-- | Provides the 'UniqueSeq' type.
module Charon.Data.UniqueSeq
  ( UniqueSeq (MkUniqueSeq),

    -- * Creation
    Internal.empty,
    Internal.singleton,
    Internal.fromFoldable,
    Internal.fromSet,

    -- * Lookup
    Internal.member,

    -- * Operations
    Internal.prepend,
    Internal.append,
    Internal.union,
    Internal.map,
  )
where

import Charon.Data.UniqueSeq.Internal (UniqueSeq (MkUniqueSeq))
import Charon.Data.UniqueSeq.Internal qualified as Internal
