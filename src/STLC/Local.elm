module STLC.Local exposing
    ( Env
    , Index
    , Level
    , Size
    , index
    , nextLevel
    , size
    )


type Index
    = Index Int


type Level
    = Level Int


type Size
    = Size Int


type alias Env entry =
    List entry


size : Env entry -> Size
size env =
    Size (List.length env)


nextLevel : Size -> Level
nextLevel (Size s) =
    Level s


index : Size -> Level -> Index
index (Size s) (Level l) =
    Index (s - (l + 1))
