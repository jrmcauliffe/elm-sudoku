module Board exposing (..)

import Dict exposing (Dict)
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


type alias Value =
    Int


type alias Square =
    ( Value, Shading )


type alias Squares =
    Dict Position Square


type alias Puzzle =
    { initial : Dict Position Value
    , rank : Int
    }


type alias Entry =
    ( Position, Value )


type alias Board =
    { squares : Squares
    , selectedSquare : Maybe Position
    , rank : Int
    }


type Shading
    = None
    | Light
    | Heavy


type alias RenderStyle =
    ( Shading, Shading )



--newBoard : Int -> Board
--newBoard rank =
--    let
--        b =
--            List.range 1 (rank * rank)
--                |> List.concatMap
--                    (\r ->
--                        List.range 1 (rank * rank)
--                            |> List.map (\c -> ( ( c, r ), ( Nothing, None ) ))
--                    )
--                |> Dict.fromList
--    in
--    { squares = b, selectedSquare = Nothing, rank = rank }


get : Position -> Board -> Maybe Square
get p b =
    Dict.get p b.squares


add : Board -> Position -> Square -> Board
add b p s =
    { b | squares = Dict.insert p s b.squares }


importPuzzle : String -> Result String Puzzle
importPuzzle s =
    let
        rawValues : List Value
        rawValues =
            s
                |> String.filter Char.isDigit
                |> String.toList
                |> List.map String.fromChar
                |> List.filterMap String.toInt

        rank : Int
        rank =
            rawValues |> List.length |> toFloat |> sqrt |> sqrt |> truncate

        indexToRC : Int -> ( Int, Int )
        indexToRC index =
            ( ((index - 1) // (rank * rank)) + 1, modBy (rank * rank) (index - 1) + 1 )

        -- TODO update this to use indexMap or something
        idx =
            List.length rawValues |> List.range 1 |> List.map indexToRC

        pairs =
            List.map2 (\a -> \b -> ( a, b )) idx rawValues

        isSquare : Bool
        isSquare =
            (rawValues |> List.length |> toFloat |> sqrt |> sqrt) == (rank |> toFloat) && (rank /= 0)

        pp =
            pairs
                |> List.filter
                    (\( _, v ) ->
                        case v of
                            0 ->
                                False

                            _ ->
                                True
                    )
                |> Dict.fromList
    in
    if isSquare then
        { initial = pp, rank = rank } |> Ok

    else
        Err "Invalid board"



--TODO change these to be rank based


getRow : Int -> List Position -> List Position
getRow r s =
    s |> List.filter (\k -> (k |> Tuple.first) == r)


getCol : Int -> List Position -> List Position
getCol c s =
    s |> List.filter (\k -> (k |> Tuple.second) == c)


getSq : Position -> Int -> List Position -> List Position
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


getPeers : Position -> Int -> List Position -> List Position
getPeers ( r, c ) rank s =
    getCol c s ++ getRow r s ++ getSq ( r, c ) rank s



-- Render a square on the board with the appropriate shading and value


renderSquare : (Position -> msg) -> Position -> ( Maybe Value, RenderStyle ) -> Svg.Svg msg
renderSquare msgOnclick p ( v, r ) =
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
            case Tuple.second r of
                Light ->
                    "#E0E0E0"

                Heavy ->
                    "#A0A0A0"

                _ ->
                    "#ffffff"

        ( val, colour ) =
            case ( v, Tuple.first r ) of
                ( Just vv, Heavy ) ->
                    ( String.fromInt vv, "black" )

                ( Just vv, Light ) ->
                    ( String.fromInt vv, "grey" )

                _ ->
                    ( "", "black" )
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


renderBoard : (Position -> msg) -> Puzzle -> List Entry -> Maybe Position -> Element msg
renderBoard msgOnclick puzzle entries selected =
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
            puzzle.rank * puzzle.rank * boxSize

        styledSquares : Dict Position ( Maybe Value, RenderStyle )
        styledSquares =
            let
                allSquares =
                    List.range 1 (puzzle.rank * puzzle.rank)
                        |> List.concatMap
                            (\r ->
                                List.range 1 (puzzle.rank * puzzle.rank)
                                    |> List.map (\c -> ( ( c, r ), ( Nothing, ( None, None ) ) ))
                            )
                        |> Dict.fromList

                problemSquares =
                    puzzle.initial
                        |> Dict.map (\_ v -> ( Just v, ( Heavy, None ) ))

                userSquares =
                    entries |> List.map (\( pp, v ) -> ( pp, ( Just v, ( Light, None ) ) )) |> Dict.fromList

                shadeSquares : Position -> Dict Position ( Maybe Value, RenderStyle ) -> Dict Position ( Maybe Value, RenderStyle )
                shadeSquares p ss =
                    let
                        peers =
                            ss |> Dict.keys |> getPeers p puzzle.rank
                    in
                    Dict.map
                        (\k ( v, shading ) ->
                            ( v
                            , if k == p then
                                ( Tuple.first shading, Heavy )

                              else if List.member k peers then
                                ( Tuple.first shading, Light )

                              else
                                shading
                            )
                        )
                        ss
            in
            allSquares
                |> Dict.union problemSquares
                |> Dict.union userSquares
                |> (case selected of
                        Just s ->
                            shadeSquares s

                        Nothing ->
                            identity
                   )

        board =
            List.range 0 ((puzzle.rank * puzzle.rank) + 1)
                |> List.concatMap
                    (renderLines puzzle.rank boxSize lineWeight heavyLineWeight)
                |> List.append (styledSquares |> Dict.map (renderSquare msgOnclick) |> Dict.values)
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
