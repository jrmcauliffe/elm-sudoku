module Board exposing (Board, Msg(..), importBoard, renderBoard, solveBoard)

import Dict
import Html exposing (Html)
import Set
import Svg
import Svg.Attributes as Att

-- TODO Work out a better way of dealing with this
dim : Int
dim = 3

type Msg
    = SolveMsg

type alias Position =
    ( Int, Int )


type alias Value =
    Int

importBoard : List Int -> Dict.Dict Position Value
importBoard l =
    let
        --
        idx =
            List.length l |> List.range 1 |> List.map (indexToRC )

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx l
                |> List.filter (\p -> Tuple.second p > 0)
    in
    pairs
        |> List.map
            (\p ->
                case Tuple.first p of
                    ( x, y ) ->
                        ( ( x, y ), Tuple.second p )
            )
        |> Dict.fromList



indexToRC : Int -> ( Int, Int )
indexToRC index =
    ( ((index - 1) // (dim * dim)) + 1, modBy (dim * dim) (index - 1) + 1 )




type alias Board = Dict.Dict Position Value



getRow : Board -> Int -> List Int
getRow b r =
    b |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.values


getCol : Board -> Int -> List Int
getCol b r =
    b |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == r) |> Dict.values


getSq : Board -> Position -> List Int
getSq b ( r, c ) =
    let
        base =
            \i -> ((i - 1) // dim) * dim

        rowNums =
            List.range 1 dim |> List.map ((+) (base r))

        colNums =
            List.range 1 dim |> List.map ((+) (base c))

        points =
            colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))
    in
    points |> List.filterMap (\v -> Dict.get v b)


remaining : Board -> Set.Set Position
remaining b =
    let
        d =
            List.range 1 (dim * dim)

        allKeys : Set.Set Position
        allKeys =
            d |> List.concatMap (\x -> d |> List.map (\y -> ( x, y ))) |> Set.fromList

        usedKeys : Set.Set Position
        usedKeys =
            b |> Dict.keys |> Set.fromList
    in
    usedKeys |> Set.diff allKeys


unSolved : Position -> Board -> Set.Set Int
unSolved ( r, c ) b =
    let
        existingVals =
            getSq b ( r, c ) |> List.append (getRow b r) |> List.append (getCol b c) |> Set.fromList

        allVals =
            List.range 1 (dim * dim) |> Set.fromList
    in
    existingVals |> Set.diff allVals


updateBoard : Board -> Position -> Int -> Board
updateBoard b p i =
    b |> Dict.insert p i


solveBoard : Board -> Board
solveBoard b =
    let
        r =
            b |> remaining

        lowPos =
            r |> Set.toList |> List.map (\p -> ( p, unSolved p b |> Set.toList )) |> List.sortBy (\( _, y ) -> y |> List.length) |> List.head
    in
    case lowPos of
        Just ( pnt, val ) ->
            updateBoard b pnt (val |> List.head |> Maybe.withDefault 0)

        Nothing ->
            b


renderNum : Position -> Value -> Svg.Svg Msg
renderNum p v =
    let
        colOffset =
            15

        rowOffset =
            38

        row =
            ((p |> Tuple.first) - 1) * 50 + rowOffset |> String.fromInt

        col =
            ((p |> Tuple.second) - 1) * 50 + colOffset |> String.fromInt
    in
    Svg.text_
        [ Att.x col
        , Att.y row
        , Att.fill "black"
        , Att.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
        ]
        [ Svg.text (String.fromInt v) ]


myLines : Int -> Int -> List (Svg.Svg Msg)
myLines size l =
    let
        sWidth =
            case modBy dim l of
                0 ->
                    "4"

                _ ->
                    "2"

        lSize =
            size * l |> String.fromInt

        bSize =
            dim * dim * size |> String.fromInt
    in
    [ Svg.line
        [ Att.x1 lSize
        , Att.y1 "0"
        , Att.x2 lSize
        , Att.y2 bSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth sWidth
        ]
        []
    , Svg.line
        [ Att.x1 "0"
        , Att.y1 lSize
        , Att.x2 bSize
        , Att.y2 lSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth sWidth
        ]
        []
    ]


renderBoard : Board -> Html Msg
renderBoard b =
    let
        -- edge of box in pixels
        boxSize =
            50

        -- edge of board in pixels
        boardSize =
            dim * dim * boxSize

        grp =
            List.range 1 (dim * dim)
                |> List.concatMap
                    (myLines boxSize)
                |> List.append ((b |> Dict.map renderNum) |> Dict.values)
                |> List.append
                    [ Svg.rect
                        [ Att.x "0"
                        , Att.y "0"
                        , boardSize |> String.fromInt |> Att.width
                        , boardSize |> String.fromInt |> Att.height
                        , Att.fill "white"
                        , Att.stroke "black"
                        , Att.strokeWidth "4"
                        ]
                        []
                    ]
                |> Svg.g [ Att.transform "translate(50,50)" ]
    in
    [ grp ]
        |> Svg.svg
            [ Att.width "800"
            , Att.height "800"
            , Att.viewBox "0 0 800 800"
            ]
