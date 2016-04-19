-- | Cached timereports
module Futurice.App.FutuHours.Timereports (
    timereports,
    addTimereports,
    ) where

import Futurice.Prelude
import Prelude          ()

import qualified Database.PostgreSQL.Simple.Fxtra as Postgres
import qualified PlanMill                         as PM

timereports
    ::Postgres.Connection
    -> PM.Interval Day
    -> PM.UserId
    -> IO PM.Timereports
timereports _conn _interval _uid = error "timereports: unimplemented"

addTimereports
    :: Foldable f
    => Postgres.Connection
    -> f PM.Timereports
    -> IO ()
addTimereports _ _ = error "addTimereports: unimplemented"
