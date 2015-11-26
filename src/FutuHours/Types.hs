{-# LANGUAGE DataKinds #-}
module FutuHours.Types (
    TimeReport,
    User,
    Holiday,
    Project,
    Task,
    ) where

import Prelude        ()
--import Prelude.Compat

import Data.Aeson.Extra (SymTag (..))

-- TODO:
type TimeReport = SymTag "TimeReport"
type User = SymTag "User"
type Holiday = SymTag "Holiday"
type Project = SymTag "Project"
type Task = SymTag "Task"
