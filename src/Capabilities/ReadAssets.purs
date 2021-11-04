module Capabilities.ReadAssets where

import Prelude

import Data.Maybe (Maybe)
import Data.Body (AssetBody)
import Node.Path (FilePath)

class Monad m <= ReadAssets m where
  readAsset :: FilePath -> m (Maybe AssetBody)
