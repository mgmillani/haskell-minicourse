module Main where

import System.Environment

parse file =
	let (result, _) = parseAux file
	in result

-- pega todas as linhas de um arquivo
parseAux [] = ([ [ [] ] ],[])
parseAux file =
	let (line,rest) = parseLine file
	    (fl,rest2) = parseAux rest
	in
		(line:fl,rest2)

-- pega todas as palavras de uma linha
parseLine [] = ([[]],[])
parseLine ('\n':rest) = ([[]],rest)
parseLine ('\r':rest) = ([[]],rest)
parseLine ln =
	let (word,rest) = parseWord ln
	    (line,rest2) = parseLine rest
	in
		 (word:line,rest2)

-- pega uma palavra da linha
parseWord [] = ([],[])
parseWord (',':rest) = ([],rest)
parseWord ('\n':rest) = ([],'\n':rest)
parseWord ('\r':rest) = ([],'\r':rest)
parseWord (h:tl) =
	let (word,rest) = parseWord tl in
		(h:word,rest)

-- projeta somete as colunas especificadas
projectColumns columns list = projectColumnsAux columns list 0
projectColumnsAux _ [] _ = []
projectColumnsAux columns (h:rest) col =
	if any (==col) columns then h:more else more
	where
		more = projectColumnsAux columns rest (col+1)

parseArgs [] = ([],[])
parseArgs (h:rest) = (h,map read rest)

main = do
	args <- getArgs
	let (filename,columns) = parseArgs args

	file <- readFile filename
	let lns = parse file

	-- separa as colunas desejadas
	let desired = map (projectColumns columns) lns

	mapM_ putStrLn $ map (\line -> (foldl (\x y -> x ++ "\t" ++ y) $ head line ) $ tail line) desired
