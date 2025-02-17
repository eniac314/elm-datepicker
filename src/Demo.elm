module Demo exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Date exposing (..)
import DatePicker exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Month(..))


type alias Model =
    { selectedDate : Maybe Date
    , datePickerData : DatePicker.Model
    }


type Msg
    = NoOp
    | DatePickerMsg DatePicker.Msg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( datePickerData, datePickerCmd ) =
            DatePicker.init "my-datepicker"
    in
    ( { datePickerData = datePickerData
      , selectedDate = Nothing
      }
    , Cmd.map DatePickerMsg datePickerCmd
    )


initFromApolloLanding : Flags -> ( Model, Cmd Msg )
initFromApolloLanding flags =
    let
        datePickerData =
            DatePicker.initFromDate "my-datepicker" (fromCalendarDate 1969 Jul 20)
    in
    ( { datePickerData = datePickerData
      , selectedDate = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DatePickerMsg datePickerMsg ->
            DatePicker.update datePickerMsg model.datePickerData
                -- set the data returned from datePickerUpdate. Don't discard the command!
                |> (\( data, cmd ) ->
                        ( { model | datePickerData = data }
                        , Cmd.map DatePickerMsg cmd
                        )
                   )
                -- and now we can respond to any internal messages we want
                |> (\( newModel, cmd ) ->
                        case datePickerMsg of
                            SubmitClicked currentSelectedDate ->
                                ( { newModel | selectedDate = Just currentSelectedDate }
                                , cmd
                                )

                            _ ->
                                ( newModel, cmd )
                   )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getDatePickerProps : DatePicker.Props
getDatePickerProps =
    let
        defaultProps =
            DatePicker.defaultProps
    in
    defaultProps


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            ]
            [ div
                [ style "margin-top" "40px"
                , style "box-shadow" "0 1px 3px rgba(0, 0, 0, 0.24)"
                ]
                [ DatePicker.view
                    model.datePickerData
                    getDatePickerProps
                    |> Html.map DatePickerMsg
                ]
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
