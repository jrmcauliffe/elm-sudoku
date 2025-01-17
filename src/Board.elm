module Board exposing (..)

import Dict exposing (Dict, map)
import Element exposing (Element)
import Svg
import Svg.Attributes as Att
import Svg.Events as Event


empty : Int -> Board
empty r =
    { squares = Dict.empty, selectedSquare = Nothing, rank = r }


type alias Row =
    Int


type alias Col =
    Int


type alias Position =
    ( Row, Col )


type Value
    = ProblemValue Int
    | UserValue Int


type alias Square =
    ( Maybe Value, Shading )


type alias Squares =
    Dict Position Square


type alias Board =
    { squares : Squares
    , selectedSquare : Maybe Position
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
                            |> List.map (\c -> ( ( c, r ), ( Nothing, None ) ))
                    )
                |> Dict.fromList
    in
    { squares = b, selectedSquare = Nothing, rank = rank }


get : Position -> Board -> Maybe Square
get p b =
    Dict.get p b.squares


add : Board -> Position -> Square -> Board
add b p s =
    { b | squares = Dict.insert p s b.squares }


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
                                ( ( x, y ), ( Just (ProblemValue value), None ) )

                            Nothing ->
                                ( ( x, y ), ( Nothing, None ) )
                    )
                |> Dict.fromList
    in
    if isSquare then
        { squares = pp, selectedSquare = Nothing, rank = rank } |> Ok

    else
        Err "Invalid board"


shadeBoard : Position -> Int -> Squares -> Squares
shadeBoard p rank s =
    let
        shadeSelected : Position -> Squares -> Squares
        shadeSelected pp ss =
            ss
                |> Dict.map
                    (\k ( v, shading ) ->
                        ( v
                        , if Just k == Just pp then
                            Heavy

                          else
                            shading
                        )
                    )

        shadePeers : Position -> Squares -> Squares
        shadePeers pp ss =
            let
                peers =
                    ss |> getPeers pp rank
            in
            ss
                |> Dict.map
                    (\k ( v, _ ) ->
                        ( v
                        , if List.member k peers then
                            Light

                          else
                            None
                        )
                    )
    in
    s |> shadePeers p |> shadeSelected p



-- |> shadeSelected p


getRow : Int -> Squares -> List Position
getRow r s =
    s |> Dict.filter (\k -> \_ -> (k |> Tuple.first) == r) |> Dict.keys


getCol : Int -> Squares -> List Position
getCol c s =
    s |> Dict.filter (\k -> \_ -> (k |> Tuple.second) == c) |> Dict.keys


getSq : Position -> Int -> Squares -> List Position
getSq ( r, c ) rank s =
    let
        base =
            \i -> ((i - 1) // rank) * rank

        rowNums =
            List.range 1 rank |> List.map ((+) (base r))

        colNums =
            List.range 1 rank |> List.map ((+) (base c))
    in
    colNums |> List.concatMap (\cc -> rowNums |> List.map (\rr -> ( rr, cc )))


getPeers : Position -> Int -> Squares -> List Position
getPeers ( r, c ) rank s =
    getCol c s ++ getRow r s ++ getSq ( r, c ) rank s



-- Render a square on the board with the appropriate shading and value


renderSquare : (Position -> msg) -> Position -> Square -> Svg.Svg msg
renderSquare msgOnclick p ( v, s ) =
    let
        -- Offsets to make the values appear in the center of the square
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
                    "#E0E0E0"

                Heavy ->
                    "#A0A0A0"

                _ ->
                    "#ffffff"

        ( val, colour ) =
            case v of
                Just (ProblemValue vv) ->
                    ( String.fromInt vv, "black" )

                Just (UserValue vv) ->
                    ( String.fromInt vv, "gray" )

                _ ->
                    ( " ", "blue" )
    in
    Svg.svg []
        [ Svg.rect
            [ Att.x (x |> String.fromInt), Att.y (y |> String.fromInt), Att.width "50", Att.height "50", Att.fill backgroundColor, Event.onClick (msgOnclick p) ]
            []
        , Svg.text_
            [ Att.x (x + yTextOffset |> String.fromInt)
            , Att.y (y + xTextOffset |> String.fromInt)
            , Att.fill colour
            , Att.style "font-family: Arial; font-size: 34;"
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

        board =
            List.range 0 ((b.rank * b.rank) + 1)
                |> List.concatMap
                    (renderLines b.rank boxSize lineWeight heavyLineWeight)
                |> List.append (b.squares |> Dict.map (renderSquare msgOnclick) |> Dict.values)
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
