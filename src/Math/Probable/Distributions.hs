module Math.Probable.Distributions where

import Math.Probable.Types
import Statistics.Distribution
import Statistics.Types

import qualified Statistics.Distribution.Beta    as Beta
import qualified Statistics.Distribution.CauchyLorentz as Cauchy
import qualified Statistics.Distribution.ChiSquared as Chi
import qualified Statistics.Distribution.Exponential as Exp
import qualified Statistics.Distribution.FDistribution as F
import qualified Statistics.Distribution.Gamma   as Gamma
import qualified Statistics.Distribution.Geometric as Geometric
import qualified Statistics.Distribution.Normal  as Normal
import qualified Statistics.Distribution.StudentT as Student
import qualified Statistics.Distribution.Uniform as Uniform

-- | Get an 'Int' following the given discrete distribution
--   designated by the 'DiscreteGen' instance you pick
discrete :: (PrimMonad m, DiscreteGen d)
         => d
         -> Prob m Int
discrete d = withGen $ genDiscreteVar d
{-# INLINE discrete #-}

-- | Get a 'Double' following the given continous distribution
--   designated by the 'ContGen' instance you pick
continuous :: (PrimMonad m, ContGen d)
           => d
           -> Prob m Double
continuous d = withGen $ genContVar d
{-# INLINE continuous #-}

-- | Uniform double between the first and second argument
uniform :: PrimMonad m
        => Double         -- ^ lower bound
        -> Double         -- ^ upper bound
        -> Prob m Double
uniform a b = continuous (Uniform.uniformDistr a b)
{-# INLINE uniform #-}

-- | Get a 'Double' from a normal distribution of the given mean and stdev
normal :: PrimMonad m
       => Double -- ^ mean
       -> Double -- ^ standard deviation
       -> Prob m Double
normal m s = continuous (Normal.normalDistr m s)
{-# INLINE normal #-}

-- | Get a 'Double' from a Beta distribution
--   using the given 'alpha' and 'beta' parameters
beta :: PrimMonad m
     => Double -- ^ shape parameter alpha
     -> Double -- ^ shape parameter beta
     -> Prob m Double
beta a b = continuous (Beta.improperBetaDistr a b)
{-# INLINE beta #-}

-- | Get a 'Double' from a Cauchy distribution 
--   using the given central point and scale
cauchy :: PrimMonad m
       => Double -- ^ central point
       -> Double -- ^ scale parameter
       -> Prob m Double
cauchy p s = continuous (Cauchy.cauchyDistribution p s)
{-# INLINE cauchy #-}

-- | Get a 'Double' using the standard Cauchy distribution
standardCauchy :: PrimMonad m
               => Prob m Double
standardCauchy = continuous Cauchy.standardCauchy
{-# INLINE standardCauchy #-}

-- | Get a 'Double' from a chi^2 distribution using 
--   the given number of degrees of freedom
chisquared :: PrimMonad m
           => Int -- ^ number of degrees of freedom
           -> Prob m Double
chisquared n = continuous (Chi.chiSquared n)
{-# INLINE chisquared #-}

-- | Get a 'Double' from a Fisher F-distribution with the given parameters
fisher :: PrimMonad m
       => Int 
       -> Int
       -> Prob m Double
fisher a b = continuous (F.fDistribution a b)
{-# INLINE fisher #-}

-- | Get a 'Double' from a gamma distribution
gamma :: PrimMonad m
      => Double -- ^ shape parameter 'k'
      -> Double -- ^ scale parameter 'theta'
      -> Prob m Double
gamma k t = continuous (Gamma.improperGammaDistr k t)
{-# INLINE gamma #-}

-- | Get an 'Int' from a geometric distribution estimating the success rate
geoInt :: PrimMonad m
       => Double -- ^ success rate
       -> Prob m Int
geoInt r = discrete (Geometric.geometric r)
{-# INLINE geoInt #-}

-- | Get a 'Double' from a geometric distribution estimating the success rate
geoDouble :: PrimMonad m
          => Double -- ^ success rate
          -> Prob m Double
geoDouble r = continuous (Geometric.geometric r)
{-# INLINE geoDouble #-}

-- | Get an 'Int' from a geometric distribution estimating the failure rate
geo0Int :: PrimMonad m
        => Double -- ^ success rate
        -> Prob m Int
geo0Int r = discrete (Geometric.geometric0 r)
{-# INLINE geo0Int #-}

-- | Get a 'Double' from a geometric distribution estimating the failure rate
geo0Double :: PrimMonad m
           => Double -- ^ success rate
           -> Prob m Double
geo0Double r = continuous (Geometric.geometric0 r)
{-# INLINE geo0Double #-}

-- | Get a 'Double' from a StudentT distribution with the given parameter
student :: PrimMonad m
        => Double
        -> Prob m Double
student tndf = continuous (Student.studentT tndf)
{-# INLINE student #-}

-- | Get a 'Double' from an exponential distribution with the given lambda parameter
exponential :: PrimMonad m
            => Double -- ^ scale parameter (lambda)
            -> Prob m Double
exponential lambda = continuous (Exp.exponential lambda)
{-# INLINE exponential #-}

-- | Get a 'Double' from an exponential distribution whose lambda
--   is the mean of the given sample.
exponentialOfSample :: PrimMonad m
                    => Sample -- ^ it will use 'exponential' with the mean of this sample
                    -> Prob m Double
exponentialOfSample s = continuous (Exp.exponentialFromSample s)
{-# INLINE exponentialOfSample #-}