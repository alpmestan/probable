-- |
-- Module       : Math.Probable
-- Copyright    : (c) 2014 Alp Mestanogullari
-- License      : BSD3
-- Maintainer   : alpmestan@gmail.com
-- Stability    : experimental
-- Portability  : GHC
-- 
-- Easy, composable and efficient random number generation,
-- with support for distributions over a finite set 
-- and your usual probability distributions.

module Math.Probable 
       ( -- * random value generation
         module Math.Probable.Random

       , -- * finite distributions
         module Math.Probable.Distribution.Finite 
       ) where

import Math.Probable.Distribution.Finite
import Math.Probable.Random

