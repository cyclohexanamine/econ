import System.Environment
import Text.Regex

endLines, fullLines :: [Regex]
endLines  = map mkRegex ["(.*)-- *\\|.*$", "(.*)-- *\\^.*$"] -- find and match "-- |" and "-- ^" comments
fullLines = map mkRegex ["^ *-- *\\|.*$\r?\n", "^ *-- *\\^.*$\r?\n"] -- as above, but matching lines which only have one of these comments, plus the newline, to be able to remove these lines completely

main = do
    args <- getArgs
    if length args >= 1 then do
        let filename = args !! 0
        fileContents <- readFile filename
        
        let fullLinesRemoved = foldl (\str rex -> subRegex rex str "") fileContents fullLines
        let inlineRemoved    = foldl (\str rex -> subRegex rex str "\\1") fullLinesRemoved endLines
        
        putStrLn inlineRemoved
        
    else
        putStrLn ""