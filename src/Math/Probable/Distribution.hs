module Math.Probable.Distribution 
    ( -- * Common distributions
      beta
    , cauchy
    , cauchyStd
    , chiSquared
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
import qualified Statistics.Distribution.ChiSquared as Chi
import qualified Statistics.Distribution.Exponential as E
import Statistics.Distribution.FDistribution
import Statistics.Distribution.Gamma
import qualified Statistics.Distribution.Geometric as G
import qualified Statistics.Distribution.Normal as N
import Statistics.Distribution.StudentT
import Statistics.Distribution.Uniform
import Statistics.Types (Sample)

-- | Sample from a continuous distribution from the 'statistics' package
-- 
-- > λ> import qualified Statistics.Distribution.Normal as Normal
-- > λ> mwc $ continuous (Normal.normalDistr 0 1)
-- > -0.7266583064693862
--
-- This is equivalent to using 'normal' from this module.
continuous :: (ContGen d, PrimMonad m) 
           => d -- ^ the continuous distribution to sample from
           -> RandT m Double
continuous d = RandT $ genContVar d
{-# INLINE continuous #-}

-- | Sample from a discrete distribution from the 'statistics' package
-- 
-- > λ> import qualified Statistics.Distribution.Normal as Normal
-- > λ> mwc $ discrete (Geo.geometric 0.6)
-- > 2
--
-- This is equivalent to using 'geometric' from this module.
discrete :: (DiscreteGen d, PrimMonad m)
          => d -- ^ the discrete distribution to sample from
          -> RandT m Int
discrete d = RandT $ genDiscreteVar d

-- | Beta distribution (from @Statistics.Distribution.Beta@)
--
-- > λ> mwc $ listOf 10 (beta 81 219)
-- > [ 0.23238372272745833,0.252972980515086,0.22708315774257903
-- > , 0.25807200295967214,0.29794072226119983,0.24534701159196015
-- > , 0.24766870269839578,0.2994199351220346,0.2728157476212405,0.2593318159573564
-- > ]
beta :: PrimMonad m 
     => Double -- ^ shape parameter alpha
     -> Double -- ^ shape parameter beta
     -> RandT m Double
beta alpha bet = continuous $ betaDistr alpha bet

-- | Cauchy distribution (from @Statistics.Distribution.Cauchy@)
-- 
-- > λ> mwc $ listOf 10 (cauchy 0 0.1)
-- > [ -0.3932758718373347,0.490467375093784,4.2620417667423555e-2
-- > , 3.370509874905657e-2,-8.186484692937862e-2,9.371858212168262e-2
-- > , -1.1095818809115384e-2,3.0353983716155386e-2,0.22759697862410477
-- > , -0.1881828277028582 ]
cauchy :: PrimMonad m
       => Double -- ^ central point
       -> Double -- ^ scale parameter
       -> RandT m Double
cauchy p l = continuous $ cauchyDistribution p l

-- | Cauchy distribution around 0, with scale 1 (from @Statistics.Distribution.Cauchy@)
-- 
-- > λ> mwc $ listOf 10 cauchyStd
-- > [ 9.409701589649838,-7.361963972107541,0.168746305673769
-- > , 5.091825420838711,-0.326080163135388,-1.2989850787629456
-- > , -2.685658063444485,0.22671438734899435,-1.602349559644217e-2
-- > , -0.6476292643908057 ]
cauchyStd :: PrimMonad m
          => RandT m Double
cauchyStd = cauchy 0 1

-- | Chi-squared distribution (from @Statistics.Distribution.ChiSquared@)
-- 
-- > λ> mwc $ listOf 10 (chiSquare 4)
-- > [ 8.068852054279787,1.861584389294606,6.3049415103095265
-- > , 1.0512164068833838,1.6243237867165086,5.284901049954076
-- > , 0.4773242487947021,1.1753876666374887,5.21554771873363
-- > , 3.477574639460651 ]
chiSquared :: PrimMonad m
           => Int -- ^ number of degrees of freedom
           -> RandT m Double
chiSquared = continuous . Chi.chiSquared

-- | Fisher's F-Distribution (from @Statistics.Distribution.FDistribution@)
--
-- > λ> mwc $ listOf 10 (fisher 4 3)
-- > [ 3.437898578540642,0.844120450719367,1.9907851466347173
-- > , 2.0089975147012784,1.3729208790549117,0.9380430357924707
-- > , 2.642389323945247,1.0918121624055352,0.45650856735477335
-- > , 2.5134537326659196 ]
fisher :: PrimMonad m
       => Int
       -> Int
       -> RandT m Double
fisher a b = continuous $ fDistribution a b

-- | Gamma distribution (from @Statistics.Distribution.Gamma@)
-- 
-- > λ> mwc $ listOf 10 (gamma 3 0.1)
-- > [ 5.683745415884202e-2,0.20726188766138176,0.3150672538487696
-- > , 0.4250825346490057,0.5586516230326105,0.46897413151474315
-- > , 0.18374916962208182,9.93000480494153e-2,0.6057279704154832
-- > , 0.11070190282993911 ]
gamma :: PrimMonad m
      => Double -- ^ shape parameter k
      -> Double -- ^ scale parameter theta
      -> RandT m Double
gamma k theta = continuous $ gammaDistr k theta

-- | Gamma distribution, without checking whether the parameter are valid
-- (from @Statistics.Distribution.Gamma@)
-- 
-- > λ> mwc $ listOf 10 (improperGamma 3 0.1)
-- > [ 0.30431838005485,0.4044380297376584,2.8950141419406657e-2
-- > , 0.468271612850147,0.18587792578128381,0.22735854572527045
-- > , 0.5168050216325927,5.896911236207261e-2,0.24654560998405564
-- > , 0.10557650513145429 ]
improperGamma :: PrimMonad m
              => Double -- ^ shape parameter k
              -> Double -- ^ scale parameter theta
              -> RandT m Double
improperGamma k theta = continuous $ improperGammaDistr k theta

-- | Geometric distribution.
-- 
-- Distribution of the number of trials needed to get one success.
-- See @Statistics.Distribution.Geometric@
--
-- > λ> mwc $ listOf 10 (geometric 0.8)
-- > [2,1,1,1,1,1,1,2,1,5]
geometric :: PrimMonad m
          => Double -- ^ success rate
          -> RandT m Int
geometric = discrete . G.geometric

-- | Geometric distribution.
--
-- Distribution of the number of failures before getting one success.
-- See @Statistics.Distribution.Geometric@
-- 
-- > λ> mwc $ listOf 10 (geometric0 0.8)
-- > [0,0,0,0,0,1,1,0,0,0]
geometric0 :: PrimMonad m
           => Double
           -> RandT m Int
geometric0 = discrete . G.geometric0

-- | Student-T distribution (from @Statistics.Distribution.StudentT@)
-- 
-- > λ> mwc $ listOf 10 (student 0.2)
-- > [ -14.221373473810829,-29.395749168822267,19.448665112984997
-- > , -30.00446058929083,-0.5033202547957609,2.321975597874013
-- > , 0.7884787761643617,-0.1895113832448149,-131.12901170537924
-- > , 1.371956948317759 ]
student :: PrimMonad m
        => Double 
        -> RandT m Double
student = continuous . studentT

-- | Uniform distribution between 'a' and 'b' (from @Statistics.Distribution.Uniform@)
--
-- > λ> mwc $ listOf 10 (uniform 0.1 0.2)
-- > [ 0.1711914559256124,0.1275212181343327,0.15347702635758945
-- > , 0.1743662387063698,0.12047749686635312,0.10719840237585587
-- > , 0.10543681342025846,0.13482973080648325,0.19779298960413577
-- > , 0.1681037592576508 ]
uniform :: PrimMonad m
        => Double
        -> Double
        -> RandT m Double
uniform a b = continuous $ uniformDistr a b

-- | Normal distribution (from @Statistics.Distribution.Normal@)
--
-- > λ> mwc $ listOf 10 (normal 4 1)
-- > [ 3.6815394812555144,3.5958531529526727,3.775960990625964
-- > , 4.413109650155896,4.825826384709198,4.805629590118984
-- > , 5.259267547365003,4.45410634165052,4.886537243027636
-- > , 3.0409409067356954 ]
normal :: PrimMonad m
       => Double -- ^ mean
       -> Double -- ^ standard deviation
       -> RandT m Double
normal mean stddev = continuous $ N.normalDistr mean stddev

-- | The standard normal distribution (mean = 0, stddev = 1) (from @Statistics.Distribution.Normal@)
-- 
-- > λ> mwc $ listOf 10 standard
-- > [ 0.2252627935262769,1.1831885443897947,-0.6577353418647461
-- > , 2.1574536855051853,-0.16983072710637676,0.9667954287638821
-- > , -1.8758605246293683,-0.8578048838241616,1.9516838769731923
-- > , 0.43752574431460434 ]
standard :: PrimMonad m
         => RandT m Double
standard = continuous N.standard

-- | Create a normal distribution using parameters estimated from the sample
-- (from @Statistics.Distribution.Normal@)
--
-- > λ> mwc . listOf 10 $ 
-- >      normalFromSample $ 
-- >        V.fromList [1,1,1,3,3,3,4
-- >                   ,4,4,4,4,4,4,4
-- >                   ,4,4,4,4,4,5,5
-- >                   ,5,7,7,7]
-- > [ 7.1837511677441395,2.388433817342809,5.252282321156134
-- > , 4.988163140851522,0.40102386713467864,4.4840751065620665
-- > , 2.1471370686776874,2.6591948802201046,3.843667372514598
-- > , 1.7650436484843248 ]
normalFromSample :: PrimMonad m
                 => Sample -- ^ sample
                 -> RandT m Double
normalFromSample = continuous . N.normalFromSample

-- | Exponential distribution (from @Statistics.Distribution.Exponential@)
-- 
-- > λ> mwc $ listOf 10 (exponential 0.2)
-- > [ 5.713524665694821,1.7774315204594584,2.434017573227628
-- > , 5.463202731505528,0.5403008025455847,14.346316301765576
-- > , 7.380393612391503,24.800854500680032,0.8731076703020924
-- > , 6.1661076502236645 ]
exponential :: PrimMonad m
            => Double -- ^ lambda (scale) parameter
            -> RandT m Double
exponential = continuous . E.exponential

-- | Exponential distribution given a sample (from @Statistics.Distribution.Exponential@)
-- 
-- > λ> mwc $ listOf 10 (exponentialFromSample $ V.fromList [1,1,1,0])
-- > [ 0.4237050903604833,1.934301502525168,0.7435728843566659
-- > , 1.8720263209574293,0.605750265970631,0.24103955067365979
-- > , 0.6294952762436511,1.660404952631443,0.6448230847113577
-- > , 0.8891555734786789 ]
exponentialFromSample :: PrimMonad m
                      => Sample
                      -> RandT m Double
exponentialFromSample = continuous . E.exponentialFromSample









