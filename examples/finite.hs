module Main where

import Math.Probable

import qualified Data.Vector as V

data Book = Interesting 
		  | Boring
	deriving (Eq, Show)

bookPrior :: Finite d => d Book
bookPrior = weighted [ (Interesting, 0.2) 
					 , (Boring, 0.8) 
					 ]

twoBooks :: Finite d => d (Book, Book)
twoBooks = do
	book1 <- bookPrior
	book2 <- bookPrior
	return (book1, book2)

sampleBooks :: RandT IO (V.Vector Book)
sampleBooks = vectorOf 10 bookPrior

oneInteresting :: Fin (Book, Book)
oneInteresting = bayes $ do
	(b1, b2) <- twoBooks
	condition (b1 == Interesting || b2 == Interesting)
	return (b1, b2)

main :: IO ()
main = do
	print $ exact bookPrior
	mwc sampleBooks >>= print
	print $ exact twoBooks
	print $ exact oneInteresting
