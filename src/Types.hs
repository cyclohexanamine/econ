module Types where


data Good = Good
    { goodName :: String
    , goodUtilityParams :: [Float]
    , goodUtilityBase :: Float
    }
    
instance Eq Good where
    g1 == g2 = (goodName g1 == goodName g2)
    
instance Show Good where 
    show = goodName
    
    
data Resource = Resource
    { resName :: String
    , resProduce :: Float -> Float
    , resDerivative :: Float -> Float
    , resRetention :: Float
    , resProduct :: Good
    }
    
instance Show Resource where 
    show res = resName res ++ "-(" ++ (show . resProduct $ res) ++ ")"
    

type GoodPrices = [(Good, Float)]
type GoodUtilities = [(Good, Float)]
type GoodQuantities = [(Good, Float)]
type GoodTrades = [(SquareRef, GoodQuantities)]
type NeighbourGoodTrades = [(SquareRef, GoodQuantities)]

type SquarePop = [(Resource, Float)]    
type SquareRef = (Int, Int) -- X, Y
type SquareNeighbour = (Neighbour, Float)
type SquareNeighbours = [SquareNeighbour]


data Square = Square
    { sqRef :: SquareRef
    , sqPop :: SquarePop
    , sqPrices :: GoodPrices
    , sqTradeIn :: GoodTrades
    , sqTradeOut :: GoodTrades
    , sqNeighbours :: SquareNeighbours
    }
    
instance Show Square where
    show square = (show . sqRef $ square) ++ "\n"
                    ++ (show . sqPop $ square) ++ "\n"
                    ++ (show . sqPrices $ square)
                    ++ (show . sqTradeOut $ square) ++ "\n"
                    ++ "\n\n"
    
data Neighbour = Neighbour
    { neighRef :: SquareRef
    , neighPrices :: GoodPrices
    , neighTradeOut :: NeighbourGoodTrades
    }
    
data World = World
    { worldSquares :: [Square]
    , worldParams :: [(String, Float)]
    }
instance Show World where
    show = show . worldSquares
    
data WorldState = WorldState
    { stateParams :: [(String, Float)]
    , stateDeltaT :: Float
    }