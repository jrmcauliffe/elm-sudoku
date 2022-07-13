module Main exposing (..)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { dimension : Int
    , contents : List Int
    }


initialModel : Model
initialModel =
    { dimension = 3
    , contents = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | dimension = model.dimension + 1 }, Cmd.none )

        Decrement ->
            ( { model | dimension = model.dimension - 1 }, Cmd.none )


myLines : Int -> Int -> Int -> List (Svg Msg)
myLines size dim l =
    let
        sWidth = case (modBy dim l ) of
            0 -> "4"
            _ -> "2"
        lSize = size * l |> String.fromInt
        bSize = dim * dim * size |> String.fromInt
    in
    [ line
        [ x1 lSize
        , y1 "0"
        , x2 lSize
        , y2 bSize
        , fill "white"
        , stroke "black"
        , strokeWidth sWidth
        ]
        []
    , line
        [ x1 "0"
        , y1 lSize
        , x2 bSize
        , y2 lSize
        , fill "white"
        , stroke "black"
        , strokeWidth sWidth
        ]
        []
    ]

board : Int -> Svg Msg
board dim =
    let
        boxSize =
            50

        boardSize =
            dim * dim * boxSize
    in
    g [ transform "translate(100,100)" ]
        (List.range 1 (dim * dim)
            |> List.concatMap
                ( myLines boxSize dim
                )
            |> List.append
                [ rect
                    [ x "0"
                    , y "0"
                    , boardSize |> String.fromInt |> width
                    , boardSize |> String.fromInt |> height
                    , fill "white"
                    , stroke "black"
                    , strokeWidth "4"
                    ]
                    []
                ]
        )


view : Model -> Html Msg
view model =
    svg
        [ width "1500"
        , height "1500"
        , viewBox "0 0 1500 1500"
        ]
        [ model.dimension |> board ]
