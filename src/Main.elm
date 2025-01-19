module Main exposing (..)

import Board as B
import Browser
import Dict
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import InputPad as I
import Puzzles as P
import Solver as S


puzzle1 =
    P.puzzles |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault ""


type alias Model =
    { puzzle : B.Puzzle
    , entries : List B.Entry
    , redos : List B.Entry
    , selectedSquare : Maybe B.Position
    , input : I.Input
    , inputString : String
    , status : String
    , assessment : S.Assessment
    }


initialModel : Model
initialModel =
    case puzzle1 |> B.importPuzzle of
        Ok p ->
            { puzzle = p
            , entries = []
            , redos = []
            , selectedSquare = Nothing
            , input = I.newInput
            , inputString = ""
            , status = ""
            , assessment = S.assess p []
            }

        Err e ->
            { puzzle = { initial = Dict.empty, rank = 3 }
            , entries = []
            , redos = []
            , selectedSquare = Nothing
            , input = I.newInput
            , inputString = ""
            , status = e
            , assessment = S.assess { initial = Dict.empty, rank = 3 } []
            }


type Msg
    = SolveMsg
    | TextMsg String
    | ButtonMsg I.Value
    | BoardMsg B.Position


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SolveMsg ->
            case model.inputString |> B.importPuzzle of
                Ok p ->
                    ( { model | puzzle = p, status = "Success!" }, Cmd.none )

                Err e ->
                    ( { model | status = e }, Cmd.none )

        TextMsg status ->
            ( { model | inputString = status }, Cmd.none )

        ButtonMsg val ->
            case ( model.selectedSquare, val ) of
                ( Just pos, I.Num i ) ->
                    ( { model | entries = ( pos, i ) :: model.entries, selectedSquare = Nothing }, Cmd.none )

                ( _, I.Undo ) ->
                    case model.entries of
                        e :: es ->
                            ( { model | entries = es, redos = e :: model.redos, selectedSquare = Nothing }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

                ( _, I.Redo ) ->
                    case model.redos of
                        r :: rs ->
                            ( { model | redos = rs, entries = r :: model.entries, selectedSquare = Nothing }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

                ( _, I.Assess ) ->
                    let
                        newAssessment =
                            S.assess model.puzzle model.entries
                    in
                    ( { model | assessment = newAssessment }, Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )

        BoardMsg pos ->
            let
                -- Only allow user squares to be selected
                newSelected =
                    case model.puzzle.initial |> Dict.member pos of
                        True ->
                            Nothing

                        False ->
                            Just pos
            in
            ( { model | selectedSquare = newSelected }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.column [ Element.width Element.fill, Element.spacing 20, Element.padding 25 ]
        [ Element.row []
            [ B.renderBoard BoardMsg model.puzzle model.entries model.selectedSquare
            , I.renderInput ButtonMsg model.input
            ]
        , Element.row [] [ Element.el [] gameInput, model.status |> Element.text, Debug.toString model.assessment |> Element.text ]
        , Element.row [] [ Element.el [] loadButton ]
        ]
        |> Element.layout []


gameInput =
    Input.multiline
        [ width <| px 450
        , height <| px 200
        , Border.rounded 6
        , Border.width 2
        , Border.color <| rgb255 0x00 0x00 0x00
        ]
        { onChange = TextMsg
        , text = ""
        , placeholder = Just <| Input.placeholder [] <| text "Enter you game here"
        , label = Input.labelAbove [] (Element.text "Game InputPad")
        , spellcheck = False
        }


loadButton =
    Input.button
        []
        { onPress = Just SolveMsg, label = Element.text "Load" }
