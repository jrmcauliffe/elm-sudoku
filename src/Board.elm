module Board exposing (..)

import Dict exposing (Dict, map)
import Element exposing (Element)
import Svg
import Svg.Attributes as Att
import Svg.Events as Event


empty : Board
empty =
    { positions = Dict.empty, selectedPosition = Nothing, rank = 3 }


type alias Row =
    Int


type alias Col =
    Int


type alias Position =
    ( Row, Col )


type alias Value =
    Int


type alias Positions =
    Dict Position (List Value)


type alias ShadedPositions =
    Dict Position ( List Value, Shading )


type alias Board =
    { positions : Positions
    , selectedPosition : Maybe Position
    , rank : Int
    }


type Shading
    = None
    | Light
    | Heavy


newBoard : Int -> Board
newBoard rank =
    let
        b =
            List.range 1 (rank * rank)
                |> List.concatMap
                    (\r ->
                        List.range 1 (rank * rank)
                            |> List.map (\c -> ( ( c, r ), List.range 1 (rank * rank) ))
                    )
                |> Dict.fromList
    in
    { positions = b, selectedPosition = Nothing, rank = rank }


add : Board -> Position -> Value -> Board
add b p v =
    { b | positions = Dict.insert p [ v ] b.positions }


importBoard : String -> Result String Board
importBoard s =
    let
        l =
            s
                |> String.filter Char.isDigit
                |> String.toList
                |> List.map String.fromChar
                |> List.map String.toInt
                |> List.map
                    (\p ->
                        case p of
                            Just 0 ->
                                Nothing

                            _ ->
                                p
                    )

        rank =
            l |> List.length |> toFloat |> sqrt |> sqrt |> truncate

        indexToRC : Int -> ( Int, Int )
        indexToRC index =
            ( ((index - 1) // (rank * rank)) + 1, modBy (rank * rank) (index - 1) + 1 )

        idx =
            List.length l |> List.range 1 |> List.map indexToRC

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx l

        isSquare =
            (l |> List.length |> toFloat |> sqrt |> sqrt) == (rank |> toFloat) && (rank /= 0)

        pp =
            pairs
                |> List.map
                    (\( ( x, y ), v ) ->
                        case v of
                            Just value ->
                                ( ( x, y ), [ value ] )

                            Nothing ->
                                ( ( x, y ), List.range 1 rank )
                    )
                |> Dict.fromList
    in
    if isSquare then
        { positions = pp, selectedPosition = Nothing, rank = rank } |> Ok

    else
        Err "Invalid board"


getRow : Int -> Board -> List Position
getRow r b =
    b.positions |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.keys


getCol : Int -> Board -> List Position
getCol c b =
    b.positions |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == c) |> Dict.keys


getSq : Board -> Position -> List Position
getSq b ( r, c ) =
    let
        base =
            \i -> ((i - 1) // b.rank) * b.rank

        rowNums =
            List.range 1 b.rank |> List.map ((+) (base r))

        colNums =
            List.range 1 b.rank |> List.map ((+) (base c))
    in
    colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))


getPeers : Position -> Board -> List Position
getPeers ( r, c ) b =
    getCol c b ++ getRow r b ++ getSq b ( r, c )


renderSquare : (Position -> msg) -> Position -> ( List Value, Shading ) -> Svg.Svg msg
renderSquare msgOnclick p ( v, s ) =
    let
        xTextOffset =
            38

        yTextOffset =
            15

        x =
            ((p |> Tuple.second) - 1) * 50

        y =
            ((p |> Tuple.first) - 1) * 50

        backgroundColor =
            case s of
                Light ->
                    "#f0f0f0"

                Heavy ->
                    "#d0d0d0"

                _ ->
                    "#ffffff"

        val =
            case v of
                [ vv ] ->
                    String.fromInt vv

                _ ->
                    " "
    in
    Svg.svg []
        [ Svg.rect
            [ Att.x (x |> String.fromInt), Att.y (y |> String.fromInt), Att.width "50", Att.height "50", Att.fill backgroundColor, Event.onClick (msgOnclick p) ]
            []
        , Svg.text_
            [ Att.x (x + yTextOffset |> String.fromInt)
            , Att.y (y + xTextOffset |> String.fromInt)
            , Att.fill "black"
            , Att.style "font-family: Arial; font-size: 34; stroke: #000000; fill: #000000;"
            , Event.onClick (msgOnclick p)
            ]
            [ Svg.text val ]
        ]


renderLines : Int -> Int -> Int -> Int -> Int -> List (Svg.Svg msg)
renderLines rank size light heavy l =
    let
        lineWidth =
            case modBy rank l of
                0 ->
                    heavy |> String.fromInt

                _ ->
                    light |> String.fromInt

        lineSize =
            size * l |> String.fromInt

        boardSize =
            rank * rank * size |> String.fromInt
    in
    [ Svg.line
        [ Att.x1 lineSize
        , Att.y1 "0"
        , Att.x2 lineSize
        , Att.y2 boardSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth lineWidth
        ]
        []
    , Svg.line
        [ Att.x1 "0"
        , Att.y1 lineSize
        , Att.x2 boardSize
        , Att.y2 lineSize
        , Att.fill "white"
        , Att.stroke "black"
        , Att.strokeWidth lineWidth
        ]
        []
    ]


renderBoard : (Position -> msg) -> Board -> Element msg
renderBoard msgOnclick b =
    let
        -- edge of number box in pixels
        boxSize =
            50

        lineWeight =
            2

        heavyLineWeight =
            lineWeight * 2

        -- edge of board in pixels
        boardSize =
            b.rank * b.rank * boxSize

        applyShading : Maybe Position -> Positions -> ShadedPositions
        applyShading p ps =
            ps
                |> Dict.map
                    (\k v ->
                        if Just k == p then
                            ( v, Heavy )

                        else
                            ( v, None )
                    )

        board =
            List.range 0 ((b.rank * b.rank) + 1)
                |> List.concatMap
                    (renderLines b.rank boxSize lineWeight heavyLineWeight)
                |> List.append (b.positions |> applyShading b.selectedPosition |> Dict.map (renderSquare msgOnclick) |> Dict.values)
                |> Svg.g [ Att.transform ("translate(" ++ (lineWeight |> String.fromInt) ++ ", " ++ (lineWeight |> String.fromInt) ++ ")") ]

        viewbox =
            "0 0 " ++ (boardSize + heavyLineWeight |> String.fromInt) ++ " " ++ (boardSize + heavyLineWeight |> String.fromInt)
    in
    [ board ]
        |> Svg.svg
            [ boardSize |> String.fromInt |> Att.width
            , boardSize |> String.fromInt |> Att.height
            , viewbox |> Att.viewBox
            ]
        |> Element.html
