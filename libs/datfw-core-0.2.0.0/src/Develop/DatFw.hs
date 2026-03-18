
--
-- @author Jordi Forga
--
module Develop.DatFw
    (-- * Foundation classes and types
      WebApp(..), HasRoute(..)
    , AuthzResult(..)
    -- * Path pieces
    , PathPiece(..), PathMultiPiece(..), showToPathPiece, readFromPathPiece
    -- * Defaults
    , guessApproot, defaultErrorHandler, authorizationCheck
    , defaultClientSessionBackend, clientSessionBackend
    -- * Re-exports
    , module Develop.DatFw.Dispatch
    , module Develop.DatFw.Handler
    , module Develop.DatFw.Widget
    )
where
import Develop.DatFw.Internal.Types
import Develop.DatFw.Internal.Classes
import Develop.DatFw.Dispatch
import Develop.DatFw.Handler
import Develop.DatFw.Widget

import Network.Wai


