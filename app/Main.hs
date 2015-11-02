import THExpander
import System.Environment 

main = do
	args <- getArgs 
	if null args then
		putStrLn "no directory specified"
	else
		expand . head $ args
