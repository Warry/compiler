module AST.Common.Located exposing
    ( Located
    , Region
    , getRegion
    , located
    , map
    , wrap2
    , unwrap
    )


type Located a
    = Located ( Region, a )


type alias Region =
    { position : ( Int, Int )
    , start : Int
    , end : Int
    }


{-| arguments order to facilitate parsing, see Parser.located
-}
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


map : (a -> b) -> Located a -> Located b
map fn (Located ( re, a )) =
    Located ( re, fn a )


wrap2 : (Located a -> Located a -> b) -> Located a -> Located a -> Located b
wrap2 fn l1 l2 =
    Located
      ( mergeRegions (getRegion l1) (getRegion l2)
      , fn l1 l2
      )


getRegion : Located a -> Region
getRegion (Located ( region, _ )) =
    region


mergeRegions : Region -> Region -> Region
mergeRegions r1 r2 =
    { position = r1.position
    , start = r1.start
    , end = r2.end
    }