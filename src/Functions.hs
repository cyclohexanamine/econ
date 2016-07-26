module Functions (loaderFunctionList) where

loaderFunctionList :: [(String, Float -> Float)]
loaderFunctionList = 
    [ ( "id" , id )
    , ( "sq" , \x -> x^2 )
    , ( "pure1", pure 1)
    ]