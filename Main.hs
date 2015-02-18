module Main where

import Valid
import System.Environment
import System.IO

main :: IO ()
main = do
		args <- getArgs
		if length args < 2 then do
			putStrLn "parameters: filename parameters"
		else do
			content <- readFile (args !! 0)
			let linesContent = lines content
			putStrLn (fromBitstring (evaluate (map tg linesContent) (map toBitstring (drop 1 args))))
			return ()
		return()