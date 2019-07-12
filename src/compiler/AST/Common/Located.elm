module AST.Common.Located exposing
    ( Located
    , Region
    , getRegion
    , located
    , unwrap
    )


type Located a
    = Located ( Region, a )


type alias Region =
    { position : ( Int, Int )
    , start : Int
    , end : Int
    }


{-| arguments order to facilitate parsing, see Parser.located -}
located : ( Int, Int ) -> Int -> a -> Int -> Located a
located position start value end =
    Located
        ( { position = position
          , start = start
          , end = end
          }
        , value
        )


unwrap : Located a -> a
unwrap (Located ( _, a )) =
    a


getRegion : Located a -> Region
getRegion (Located ( region, _ )) =
    region
