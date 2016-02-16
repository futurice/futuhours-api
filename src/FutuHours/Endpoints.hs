{-# LANGUAGE RecordWildCards #-}

-- | API endpoints
module FutuHours.Endpoints (
    Context(..),
    getProjects,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (nub)
import Database.PostgreSQL.Simple (Connection)
import Data.Pool (Pool)

import qualified Data.Vector as V

import FutuHours.Types

-- Planmill modules
import qualified PlanMill                       as PM (Cfg (..),
                                                       Identifier (..),
                                                       PlanMill)
import qualified PlanMill.EndPoints.Assignments as PM (ReportableAssignment (..),
                                                       ReportableAssignments,
                                                       reportableAssignments)
import qualified PlanMill.Test                  as PM (evalPlanMillIO)

-- | We probably will have some
data Context = Context
    { ctxPlanmillCfg  :: !PM.Cfg
    , ctxPostgresPool :: !(Pool Connection)
    }

-- | Return projects for user
--
-- TODO: Add short living cache (15min?)
-- TODO: see <https://github.com/futurice/futuhours-api/issues/1>
getProjects :: MonadIO m => Context -> UserId -> m (V.Vector Project)
getProjects Context { ctxPlanmillCfg = cfg } (UserId uid) =
    liftIO $ nubVector . fmap pmToFh <$> PM.evalPlanMillIO cfg planmill
  where
    -- TODO: there is somewhere better one, I'm sure.
    nubVector :: Eq a => V.Vector a -> V.Vector a
    nubVector = V.fromList . nub . V.toList

    planmill :: PM.PlanMill PM.ReportableAssignments
    planmill = PM.reportableAssignments $ PM.Ident $ fromIntegral uid

    pmToFh :: PM.ReportableAssignment -> Project
    pmToFh PM.ReportableAssignment{..} = Project raProject raProjectName
