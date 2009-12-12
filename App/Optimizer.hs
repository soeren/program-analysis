-- Optimizer module
-- Executes Constant Folding and Dead Code Elimination
-- sequentially until no furhter change are possible in
-- order to minimize the program as far as possible
module App.Optimizer where

import DataFlow.Generic (hasChangedP)
import Optimizer.DeadCode
import Optimizer.ConstFold

optimize p = if hasChangedP p p' then optimize p' else p
    where
      p' = opt p
      opt = deadCode . constFold
