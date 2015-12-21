{-# LANGUAGE RecordWildCards #-}

-- | API endpoints
module FutuHours.Endpoints (
    Context,
    getProjects,
    ) where

import Prelude        ()
import Prelude.Compat

import qualified Data.Vector as V

import FutuHours.Types

-- Planmill modules
import qualified PlanMill                    as PM (Cfg (..))
import qualified PlanMill.EndPoints.Projects as PM (Project (..), Projects (..),
                                                    projects)
import qualified PlanMill.Test               as PM (evalPlanMillIO)

-- | We probably will have some
type Context = PM.Cfg

-- | Return projects for user
--
-- TODO: see <https://github.com/futurice/futuhours-api/issues/1>
getProjects :: Context -> IO (V.Vector Project)
getProjects cfg = fmap pmToFh . getProjects' <$> PM.evalPlanMillIO cfg PM.projects
  where
    getProjects' (PM.Projects ps) = ps
    pmToFh PM.Project{..} = Project pId pName
