module Loader (loadConfFile) where

import Text.Parsec
import Data.List
import Types
import Functions


loadConfFile :: String -> IO (Either String World)
loadConfFile fName = do
    contents <- readFile fName
    let res = runParser initFile () fName contents
    return $ case res of Left err -> Left . show $ err
                         Right w  -> Right w



data PResource = PResource
    { pResName :: String
    , pResProduct :: String
    , pResProduce :: String
    , pResDerivative :: String
    , pResRetention :: Float
    }

data PSquare = PSquare
    { pSqRef :: String
    , pSqPop :: [(String, Float)]
    , pSqPrices :: [(String, Float)]
    , pSqTradeIn :: [(String, [(String, Float)])]
    , pSqTradeOut :: [(String, [(String, Float)])]
    , pSqNeighbours :: ([(String, Float)], Maybe Float)
    }

data ISquare = ISquare
    { iSqRef :: SquareRef
    , iSqPop :: SquarePop
    , iSqPrices :: GoodPrices
    , iSqTradeIn :: GoodTrades
    , iSqTradeOut :: GoodTrades
    , iSqNeighbours :: ([(SquareRef, Float)], Maybe Float)
    }

wsc = many (ws1 <|> comment)
wsp = many space
ws1 = many1 space
comment = string "--" >> many (noneOf "\n\r") >>= \x -> endOfLine >> return x
nameStr = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['\'']
float = do
    x <- many1 (oneOf ['0'..'9'])
    y <- optionMaybe $ string "." >> many (oneOf ['0'..'9'])
    return . read $ x ++ case y of Just st -> '.':st
                                   Nothing -> ""
    
    
initFile :: Parsec String () World
initFile = do
    varList <- varSection
    goodList <- goodSection
    pResourceList <- resourceSection
    pSquareList <- squareSection
    
    let resourceList = map (parseResource goodList) pResourceList
    let iSquareList = map (parseSquare goodList resourceList) pSquareList
    let squareList = map (linkNeighbours iSquareList) iSquareList
    
    return $ World squareList varList
  
listObj vp = do
    wsp
    k <- name
    wsp
    char ':'
    wsp
    v <- vp
    wsp
    return (k, v)
    
colonList :: Parsec String () a -> Parsec String () [(String, a)]
colonList vp = do
    char '['
    l <- sepBy (listObj vp) (wsp >> char ',' >> wsp)
    char ']'
    return l
    
cFList = colonList float
cNList = colonList . colonList $ float
    
    
    
varSection :: Parsec String () [(String, Float)]
varSection = do
    wsc
    string "#VARIABLES"
    many . try $ variable 
    
variable = do
    wsc
    n <- varName
    wsp
    char '='
    wsp
    v <- float
    return (n, v)
    
varName = do
    c <- oneOf ['a'..'z']
    rest <- many . oneOf $ nameStr
    return $ c:rest
    
name = many1 . oneOf $ nameStr
    

goodSection :: Parsec String () [Good]
goodSection = do
    wsc
    string "#GOODS"
    many . try $ good
    
good = do
    wsc
    n <- name
    ws1
    params <- between (char '[') (char ']') (many . noneOf $ "[]")
    let paramL = read $ "[" ++ params ++ "]" :: [Float]
    ws1
    base <- float
    return $ Good n paramL base
    

resourceSection :: Parsec String () [PResource]
resourceSection = do
    wsc
    string "#RESOURCES"
    many . try $ resource
    
resource = do
    wsc
    n <- name
    ws1
    g <- name
    ws1
    pF <- name
    ws1 
    dF <- name
    ws1
    r <- float
    return $ PResource n g pF dF r
    

squareSection :: Parsec String () [PSquare]
squareSection = do
    wsc
    string "#SQUARES"
    many . try $ square
    
square = do
    wsc
    ref <- between (char '(') (char ')') (many . noneOf $ "()") >>= \x -> return $ "(" ++ x ++ ")"
    ws1
    pop <- cFList
    ws1
    pr <- cFList
    ws1 
    it <- cNList
    ws1 
    out <- cNList
    ws1 
    cn <- cFList
    let cn' = case find ((=="ALL").fst) cn of Just (a,v) -> (cn\\[(a,v)], Just v)
                                              Nothing    -> (cn, Nothing)
    return $ PSquare ref pop pr it out cn'
    
    
    
parseResource :: [Good] -> PResource -> Resource
parseResource goodList pr = 
    let newName = pResName pr
        newProduce = findFunc . pResProduce $ pr
        newDerivative = findFunc . pResDerivative $ pr
        newRetention = pResRetention pr
        newProduct = findGood goodList . pResProduct $ pr
    in  Resource newName newProduce newDerivative newRetention newProduct

parseSquare :: [Good] -> [Resource] -> PSquare -> ISquare
parseSquare goodList resourceList ps =
    let newRef = read (pSqRef ps) :: SquareRef
        newPop = map (\(r, p) -> (findRes resourceList r, p)) . pSqPop $ ps
        newPrices = map (\(g, p) -> (findGood goodList g, p)) . pSqPrices $ ps
        newTradeIn = resolveTrades goodList . pSqTradeIn $ ps
        newTradeOut = resolveTrades goodList . pSqTradeOut $ ps
        newSquareNeighbours = (map (\(r, c) -> (read r :: SquareRef, c)) . fst . pSqNeighbours $ ps,  snd . pSqNeighbours $ ps)
    in  ISquare newRef newPop newPrices newTradeIn newTradeOut newSquareNeighbours
    
    
linkNeighbours :: [ISquare] -> ISquare -> Square
linkNeighbours isql isq =
    let linkedNL = linkRemaining isql isq
        newNeighbours = map (\(r, c) -> (makeNeighbour . findISq isql $ r, c)) linkedNL
    in  Square (iSqRef isq) (iSqPop isq) (iSqPrices isq) (iSqTradeIn isq) (iSqTradeOut isq) newNeighbours  
        
linkRemaining :: [ISquare] -> ISquare -> [(SquareRef, Float)]
linkRemaining isql isq = 
    let oldNeighbours = fst . iSqNeighbours $   isq
        
        (x, y) = iSqRef isq
        fmax max = filter (>=0) . filter (<= max)
        (xMax, yMax) = foldl (\(mx, my) sq -> let (x, y) = iSqRef sq in (max mx x, max my y)) (0, 0) isql
        
        allRefs = (\x y -> (x, y)) <$> fmax xMax [x-1, x, x+1] <*> fmax yMax [y-1, y, y+1]
        neighbourRefs = allRefs \\ [(x, y)]
        remainingRefs = filter (\r -> not . elem r . map fst $ oldNeighbours) neighbourRefs
        
        newNeighbours = map (\r -> let (Just defaultV) = snd . iSqNeighbours $ isq in (r, defaultV)) remainingRefs
    in  oldNeighbours ++ newNeighbours
        


findGood :: [Good] -> String -> Good
findGood gl n = head . filter ((==n).goodName) $ gl

findRes :: [Resource] -> String -> Resource
findRes rl n = head . filter ((==n).resName) $ rl

findISq :: [ISquare] -> SquareRef -> ISquare
findISq isql n = head . filter ((==n).iSqRef) $ isql

findFunc :: String -> Float -> Float
findFunc n = snd . head . filter ((==n).fst) $ loaderFunctionList


resolveTrades :: [Good] -> [(String, [(String, Float)])] -> GoodTrades
resolveTrades gl tl = map (\(ref, trades) -> (read ref :: SquareRef, map (\(g, t) -> (findGood gl g, t)) trades)) tl


makeNeighbour :: ISquare -> Neighbour
makeNeighbour iSquare =
    Neighbour (iSqRef iSquare) (iSqPrices iSquare) (iSqTradeOut iSquare)
