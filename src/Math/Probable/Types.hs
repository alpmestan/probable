module Math.Probable.Types
	( 
	  -- * Prob type 
	  Prob, 
	  runP,
	  
	  -- * accessing the random generator
	  withGen,
	  PrimMonad,
	  PrimState,
	  Gen,
	) 
	where

import Control.Applicative
import Control.Monad.Primitive
import System.Random.MWC

newtype Prob m a = 
	Prob { 
		runP :: Gen (PrimState m) -> m a 
	}

withGen :: PrimMonad m
		=> (Gen (PrimState m) -> m a)
		-> Prob m a
withGen act = Prob $ act

instance Functor m => Functor (Prob m) where
	fmap f p = 
		Prob $ \gen -> fmap f $ runP p gen

instance Applicative m => Applicative (Prob m) where
	pure x = Prob $ const (pure x)

	f <*> x = Prob $ \gen -> 
		runP f gen <*> runP x gen

instance Monad m => Monad (Prob m) where
	return x = Prob $ const (return x)

	p >>= k  = Prob $ \gen -> 
			     do v <- runP p gen
			     	runP (k v) gen
