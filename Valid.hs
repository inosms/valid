module Valid where

import Test.QuickCheck
import Data.List
import Data.Char

data Bit = L | R deriving (Eq, Show)
type Bitstring = [Bit]

data GeneComponent = GeneOperator Char | GeneParameter Int deriving Show
type Gene = [GeneComponent]
type Genome = [Gene]

toGene :: String -> Gene
toGene (x:xs) 
	| isAlpha x = (GeneOperator x) : toGene xs
	| otherwise = (GeneParameter (digitToInt x)) : toGene xs
toGene [] = []
tg = toGene

toBitstring :: String -> Bitstring
toBitstring ('0':xs) = L:(toBitstring xs)
toBitstring ('1':xs) = R:(toBitstring xs)
toBitstring (_:xs) = toBitstring xs
toBitstring [] = []

fromBitstring :: Bitstring -> String
fromBitstring bitstring = map (\x->if x == L then '0' else '1') bitstring

plus :: Bitstring -> Bitstring -> Bitstring
plus a b =  _plus a b L
_plus :: Bitstring -> Bitstring -> Bit -> Bitstring
_plus (R:xs) (L:ys) L = R:(_plus xs ys L)
_plus (R:xs) (L:ys) R = L:(_plus xs ys R)
_plus (R:xs) (R:ys) L = L:(_plus xs ys R)
_plus (R:xs) (R:ys) R = R:(_plus xs ys R)
_plus (L:xs) (R:ys) L = R:(_plus xs ys L)
_plus (L:xs) (R:ys) R = L:(_plus xs ys R)
_plus (L:xs) (L:ys) L = L:(_plus xs ys L)
_plus (L:xs) (L:ys) R = R:(_plus xs ys L)
_plus xs [] R = _plus xs [R] L
_plus xs [] L = xs
_plus [] ys R = _plus ys [R] L
_plus [] ys L = ys

binaryToInt :: Bitstring -> Int
binaryToInt bitstring = sum [if (bitstring !! x) == R then 2^x else 0|x<-[0..((length bitstring)-1)]]

neg :: Bitstring -> Bitstring
neg (L:xs) = R:(neg xs)
neg (R:xs) = L:(neg xs)
neg [] = []

seq_neg :: Bitstring -> Bitstring
seq_neg input = evaluate [tg "e0ci0njct0pjct0"] [input]


getCorrectParameter :: Int -> [Bitstring] -> Bitstring
getCorrectParameter paramNumber parameters 
	| paramNumber < (length parameters) = (!!) parameters paramNumber 
	| otherwise = []


-- evaluates the genome, this takes the first gene, evaluates this with the
-- rest of the gnome as jump library
-- the second parameter is a list of parameters for the genome
evaluate :: Genome -> [Bitstring] -> Bitstring
evaluate [] _ = []
evaluate genome@(x:xs) params = fst( evaluateGene genome x params )

bitstringTail :: Bitstring -> Bitstring
bitstringTail [] = []
bitstringTail (x:xs) = xs

getParamsCount :: Gene -> Int
getParamsCount [] = 0
getParamsCount ((GeneParameter x):xs) = max (x+1) (getParamsCount xs)
getParamsCount ((GeneOperator _):xs) = getParamsCount xs 

evaluateGene :: Genome -> Gene -> [Bitstring] -> (Bitstring,Gene)
evaluateGene genome ((GeneOperator '+'):xs) parameters = evaluateWrapper genome (\[x,y] -> plus x y) 2 xs parameters
evaluateGene genome ((GeneOperator 'i'):xs) parameters = evaluateWrapper genome (\[x,y,z] -> if null x || head x == L then z else y) 3 xs parameters
evaluateGene genome ((GeneOperator 'e'):xs) parameters = evaluateWrapper genome (\[x,y,z] -> if null x then y else z) 3 xs parameters
evaluateGene genome ((GeneOperator 't'):xs) parameters = evaluateWrapper genome (bitstringTail.head) 1 xs parameters
evaluateGene genome ((GeneOperator 'p'):xs) parameters = evaluateWrapper genome ((R:).head) 1 xs parameters
evaluateGene genome ((GeneOperator 'r'):xs) parameters = evaluateWrapper genome (reverse.head) 1 xs parameters
evaluateGene genome ((GeneOperator 'n'):xs) parameters = evaluateWrapper genome ((L:).head) 1 xs parameters
evaluateGene genome ((GeneOperator 'a'):xs) parameters = evaluateWrapper genome (concat) 2 xs parameters
evaluateGene genome ((GeneOperator 'c'):xs) parameters = ([],xs)
evaluateGene genome ((GeneOperator 'j'):xs) parameters = evaluateWrapper genome (\x->fst( evaluateGene genome destGene x )) destGeneParamsCount destGeneRest parameters
	where (destGeneIndex,destGeneRest) = evaluateWrapper genome (head) 1 xs parameters
	      destGeneIndexInt = (binaryToInt destGeneIndex) `mod` (length genome)
	      destGene = genome !! destGeneIndexInt
	      destGeneParamsCount = getParamsCount destGene
evaluateGene genome ((GeneParameter x):xs) parameters = (getCorrectParameter x parameters, xs)
evaluateGene genome [] _ = ([],[])

evaluateWrapper :: Genome -> ([Bitstring]->Bitstring) -> Int -> Gene -> [Bitstring] -> (Bitstring,Gene)
evaluateWrapper genome func paramsCount gene params = (func results,rest)
				where (results,rest) = evaluateNTimes genome gene params paramsCount


-- takes a gene, parameters and an integer which specifies how often the rest after
-- one successfull evaluation is evaluated
-- the result of the evaluations are then given back
evaluateNTimes :: Genome -> Gene -> [Bitstring] -> Int -> ([Bitstring],Gene)
evaluateNTimes genome xs parameters 1 = ([value1],rest1)
				where (value1,rest1) = evaluateGene genome xs parameters 
evaluateNTimes genome xs parameters n = (value1:resultValues,resultRest)
				where (value1,rest1) = evaluateGene genome xs parameters 
				      (resultValues,resultRest) = evaluateNTimes genome rest1 parameters (n-1)
