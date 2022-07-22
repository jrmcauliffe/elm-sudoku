module Main exposing (..)

import Board as B
import Browser
import Element exposing (Element)
import Element.Input
import Html exposing (Html)



-- Puzzle from Brain Training 1


puzzle1 =
    [ 1
    , 6
    , 0
    , 4
    , 0
    , 9
    , 8
    , 0
    , 0
    , 4
    , 2
    , 0
    , 0
    , 0
    , 1
    , 5
    , 0
    , 0
    , 0
    , 0
    , 5
    , 8
    , 0
    , 0
    , 0
    , 4
    , 3
    , 5
    , 0
    , 2
    , 7
    , 0
    , 8
    , 0
    , 9
    , 1
    , 0
    , 0
    , 0
    , 0
    , 9
    , 0
    , 0
    , 0
    , 0
    , 8
    , 7
    , 0
    , 1
    , 0
    , 3
    , 2
    , 0
    , 6
    , 9
    , 3
    , 0
    , 0
    , 0
    , 5
    , 6
    , 0
    , 0
    , 0
    , 0
    , 1
    , 9
    , 0
    , 0
    , 0
    , 3
    , 2
    , 0
    , 0
    , 4
    , 6
    , 0
    , 7
    , 0
    , 1
    , 5
    ]


type alias Model =
    { b : B.Board
    }


initialModel : Model
initialModel =
    { b = puzzle1 |> B.importBoard }


main : Program () Model B.Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : B.Msg -> Model -> ( Model, Cmd B.Msg )
update msg model =
    let
        oldB =
            model.b
    in
    case msg of
        B.SolveMsg ->
            ( { model | b = B.solveBoard oldB }, Cmd.none )


view : Model -> Html B.Msg
view model =
    Element.column [ Element.explain Debug.todo, Element.width Element.fill, Element.height Element.fill, Element.spacing 5 ]
        [ Element.row [] [ model.b |> B.renderBoard |> Element.html ], Element.row [ Element.centerX ] [ Element.el [] myButton ] ]
        |> Element.layout []


myButton =
    Element.Input.button [] { onPress = Just B.SolveMsg, label = Element.text "Click Me" }
