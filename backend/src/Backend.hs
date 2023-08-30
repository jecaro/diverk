module Backend (backend) where

import Common.Route (BackendRoute, FrontendRoute, fullRouteEncoder)
import Obelisk.Backend (Backend (..))

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> serve . const $ pure (),
      _backend_routeEncoder = fullRouteEncoder
    }
