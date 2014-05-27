module Math.Probable.Distribution 
    ( -- * Common distributions
	  beta
	, cauchy
	, cauchyStd
	, chiSquare
	, fisher
	, gamma
	, improperGamma
	, geometric
	, geometric0
	, student
	, uniform
	, normal
	, standard
	, normalFromSample
	, exponential
	, exponentialFromSample

   	 -- * Finite distributions 
   , module Math.Probable.Distribution.Finite

   	 -- * Utility functions
   , continuous
   , discrete

   ) where

import Control.Monad.Primitive
import Math.Probable.Distribution.Finite
import Math.Probable.Random

import Statistics.Distribution (ContGen, DiscreteGen, genContVar, genDiscreteVar)
import Statistics.Distribution.Beta
import Statistics.Distribution.CauchyLorentz
import Statistics.Distribution.ChiSquared
import qualified Statistics.Distribution.Exponential as E
import Statistics.Distribution.FDistribution
import Statistics.Distribution.Gamma
import qualified Statistics.Distribution.Geometric as G
import qualified Statistics.Distribution.Normal as N
import Statistics.Distribution.StudentT
import Statistics.Distribution.Uniform
import Statistics.Types (Sample)

-- | Sample from a continuous distribution from the 'statistics' package
continuous :: (ContGen d, PrimMonad m) 
		   => d -- ^ the continuous distribution to sample from
		   -> RandT m Double
continuous d = RandT $ genContVar d
{-# INLINE continuous #-}

-- | Sample from a discrete distribution from the 'statistics' package
discrete :: (DiscreteGen d, PrimMonad m)
		  => d -- ^ the discrete distribution to sample from
		  -> RandT m Int
discrete d = RandT $ genDiscreteVar d

-- | Beta distribution (from @Statistics.Distribution.Beta@)
beta :: PrimMonad m 
	 => Double -- ^ shape parameter alpha
	 -> Double -- ^ shape parameter beta
	 -> RandT m Double
beta alpha bet = continuous $ betaDistr alpha bet

-- | Cauchy distribution (from @Statistics.Distribution.Cauchy@)
cauchy :: PrimMonad m
	   => Double -- ^ central point
	   -> Double -- ^ scale parameter
	   -> RandT m Double
cauchy p l = continuous $ cauchyDistribution p l

-- | Cauchy distribution around 0, with scale 1 (from @Statistics.Distribution.Cauchy@)
cauchyStd :: PrimMonad m
		  => RandT m Double
cauchyStd = cauchy 0 1

-- | Chi-squared distribution (from @Statistics.Distribution.ChiSquared@)
chiSquare :: PrimMonad m
	 	  => Int -- ^ number of degrees of freedom
	 	  -> RandT m Double
chiSquare = continuous . chiSquared

-- | Fisher's F-Distribution (from @Statistics.Distribution.FDistribution@)
fisher :: PrimMonad m
	   => Int
	   -> Int
	   -> RandT m Double
fisher a b = continuous $ fDistribution a b

-- | Gamma distribution (from @Statistics.Distribution.Gamma@)
gamma :: PrimMonad m
	  => Double -- ^ shape parameter k
	  -> Double -- ^ scale parameter theta
	  -> RandT m Double
gamma k theta = continuous $ gammaDistr k theta

-- | Gamma distribution, without checking whether the parameter are valid
-- (from @Statistics.Distribution.Gamma@)
improperGamma :: PrimMonad m
			  => Double -- ^ shape parameter k
			  -> Double -- ^ scale parameter theta
			  -> RandT m Double
improperGamma k theta = continuous $ improperGammaDistr k theta

-- | Geometric distribution.
-- 
-- Distribution of the number of trials needed to get one success.
-- See @Statistics.Distribution.Geometric@
geometric :: PrimMonad m
		  => Double -- ^ success rate
		  -> RandT m Int
geometric = discrete . G.geometric

-- | Geometric distribution.
--
-- Distribution of the number of failures before getting one success.
-- See @Statistics.Distribution.Geometric@
geometric0 :: PrimMonad m
		   => Double
		   -> RandT m Int
geometric0 = discrete . G.geometric0

-- | Student-T distribution (from @Statistics.Distribution.StudentT@)
student :: PrimMonad m
	    => Double 
	    -> RandT m Double
student = continuous . studentT

-- | Uniform distribution between 'a' and 'b' (from @Statistics.Distribution.Uniform@)
uniform :: PrimMonad m
		=> Double
		-> Double
		-> RandT m Double
uniform a b = continuous $ uniformDistr a b

-- | Normal distribution (from @Statistics.Distribution.Normal@)
normal :: PrimMonad m
	   => Double -- ^ mean
	   -> Double -- ^ standard deviation
	   -> RandT m Double
normal mean stddev = continuous $ N.normalDistr mean stddev

-- | The standard normal distribution (mean = 0, stddev = 1) (from @Statistics.Distribution.Normal@)
standard :: PrimMonad m
		 => RandT m Double
standard = continuous N.standard

-- | Create a normal distribution using parameters estimated from the sample
-- (from @Statistics.Distribution.Normal@)
normalFromSample :: PrimMonad m
				 => Sample -- ^ sample
				 -> RandT m Double
normalFromSample = continuous . N.normalFromSample

-- | Exponential distribution (from @Statistics.Distribution.Exponential@)
exponential :: PrimMonad m
			=> Double -- ^ lambda (scale) parameter
			-> RandT m Double
exponential = continuous . E.exponential

-- | Exponential distribution given a sample (from @Statistics.Distribution.Exponential@)
exponentialFromSample :: PrimMonad m
					  => Sample
					  -> RandT m Double
exponentialFromSample = continuous . E.exponentialFromSample









