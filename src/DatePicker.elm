module DatePicker exposing
    ( Msg(..), init, initFromDate, update, Model
    , view, Props, defaultProps
    , setIndexDate
    , SelectionMode
    )

{-| This module provides a styled date picker for Elm.
[You can check out the demo here.](http://abradley2.github.io/elm-datepicker/)

To alter the color theme edit `./styl/Variables.styl`, then run
`npm install && npm run build-styles`.

This library depends heavily on [justinmimbs/date](https://package.elm-lang.org/packages/justinmimbs/date/3.1.2/) as
this provides a nice RD based wrapper around Dates that is agnostic to time/timezone, which is better suited for
calendars. See the documentation there for any specific handling of the `Date` type.


# Tea / Initialization

@docs Msg, init, initFromDate, update, Model


# Rendering and Settings

@docs view, Props, defaultProps


# Helpers

@docs setIndexDate


# Types

@docs SelectionMode

-}

import Browser.Dom as Dom
import Color
import Date exposing (..)
import DatePicker.Util exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Element.Region as Region
import Html as Html
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvents
import Html.Keyed
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Simple.Transition as Transition
import Task
import Time exposing (Month(..), Weekday(..))


{-| Represents the current mode the picker is set to

    type SelectionMode
        = Calendar
        | YearPicker

-}
type SelectionMode
    = Calendar
    | YearPicker


type MonthChange
    = None
    | Previous
    | Next


{-| You will first need to add the `DatePicker.Msg` to the type consumed by your `update` function so
it recognizes this type.

    import DatePicker
    ...
    type Msg
        = FireZeMissiles
        | DatePickerMsg DatePicker.Msg

-}
type Msg
    = NoOp
    | DateSelected Date Date
    | GetToday Date
    | PreviousMonth Date
    | NextMonth Date
    | SubmitClicked Date
    | CancelClicked
    | SetSelectionMode SelectionMode


{-| The `DatePicker.Model` type needs to be added to any data structure that requires a picker instance

    import DatePicker
    ...
    type alias Model =
        { datePickerData : DatePicker.Model
        }
    }

This is mostly an opaque type you don't have to worry about, though there are some important fields you will
want to use:

  - `today` is the default "selected" day of the picker before the user has actually clicked to "select" a day.
    This is needed so the head display isn't empty before the user has selected anything, without forcing there be a default selected date of "today".
  - `indexDate` is a date used to track which month the calendar is currently showing. Do not set this directly. Use the `setIndexDate` helper
  - `selectedDate` is the last date the user clicked on in the calendar that was selectable
  - `selectionMode` determines whether the user sees the `Calendar` or the `YearPicker`

-}
type alias Model =
    { id : String
    , today : Maybe Date
    , indexDate : Maybe Date
    , currentMonthMap : Maybe (List ( Int, Date ))
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , monthChange : MonthChange
    , selectionMode : SelectionMode
    , yearList : Maybe (List Int)
    }


type alias InitializedModel =
    { id : String
    , today : Date
    , indexDate : Date
    , currentMonthMap : List ( Int, Date )
    , previousMonthMap : Maybe (List ( Int, Date ))
    , selectedDate : Maybe Date
    , previousSelectedDate : Maybe Date
    , monthChange : MonthChange
    , selectionMode : SelectionMode
    , yearList : List Int
    }


{-| Takes any of type `DatePicker.Model` and returns a new one with the given index date. It is
important to not just set indexDate directly as this will not refresh the data to completely
reflect this
-}
setIndexDate : Model -> Date -> Model
setIndexDate model indexDate =
    let
        lastDayOfMonth =
            getLastDayOfMonth indexDate (Date.day indexDate)

        monthMap =
            buildMonthMap
                []
                1
                lastDayOfMonth
                (setDayOfMonth indexDate 1)
                indexDate
    in
    { model
        | indexDate =
            -- this will set it to beginning of day
            Just (setDayOfMonth indexDate (Date.day indexDate))
        , currentMonthMap = Just monthMap

        -- only set previous month map if index date is of different month
        , previousMonthMap =
            Maybe.map
                (\prevIndexDate ->
                    if isNewMonth indexDate prevIndexDate then
                        model.currentMonthMap

                    else
                        Nothing
                )
                model.indexDate
                |> Maybe.withDefault Nothing
    }


isNewMonth : Date -> Date -> Bool
isNewMonth a b =
    Date.month a /= Date.month b || Date.year a /= Date.year b


{-| `DatePicker.init` returns an initialized record of `DatePicker.Model`. Do not throw out the returned command!
The command is used to get today's current date which the date picker uses as the default for display.
The string passed as the first argument must be a unique `id` for the date picker.

    import DatePicker

    init : ( Model, Cmd Msg )
    init =
        let
            ( datePickerData, datePickerInitCmd ) =
                DatePicker.init "my-datepicker-id"
        in
        ( { datePickerData = datePickerData
          , selectedDate = Nothing
          }
        , Cmd.map DatePickerMsg datePickerInitCmd
        )

-}
init : String -> ( Model, Cmd Msg )
init id =
    ( { id = id
      , today = Nothing
      , indexDate = Nothing
      , currentMonthMap = Nothing
      , previousMonthMap = Nothing
      , selectedDate = Nothing
      , previousSelectedDate = Nothing
      , monthChange = None
      , selectionMode = Calendar
      , yearList = Nothing
      }
    , Task.perform GetToday Date.today
    )


{-| Initialize a date picker given a date to start from.
-}
initFromDate : String -> Date -> Model
initFromDate id date =
    setIndexDate
        { id = id
        , today = Just date
        , indexDate = Nothing
        , currentMonthMap = Nothing
        , previousMonthMap = Nothing
        , selectedDate = Nothing
        , previousSelectedDate = Nothing
        , monthChange = None
        , selectionMode = Calendar
        , yearList = Just (defaultedYearList Nothing date)
        }
        date


{-| Use `DatePicker.update` to create updated date picker models from any message events.
For a nice full working example check out the [demo source here](https://github.com/abradley2/elm-datepicker/blob/master/src/Demo.elm)

    import DatePicker exposing (Msg(..))
    ...
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NoOp ->
                ( model, Cmd.none )

        DatePickerMsg datePickerMsg ->
            let
                (updatedPicker, pickerCmd) =
                    DatePicker.update
                        datepickerMsg
                        model.datePickerData
            in
                ({ model
                 | datePickerData = updatedPicker
                 , selectedDate = datePickerData.selectedDate
                }
                , Cmd.map DatePickerMsg pickerCmd
                )

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DateSelected date previousDate ->
            let
                newModel =
                    setIndexDate model date
            in
            ( { newModel
                | selectedDate = Just date
                , previousSelectedDate = Just previousDate
                , selectionMode = Calendar
              }
            , Cmd.none
            )

        NextMonth newIndexDate ->
            let
                newModel =
                    setIndexDate model newIndexDate
            in
            ( { newModel
                | monthChange = Next
              }
            , Cmd.none
            )

        PreviousMonth newIndexDate ->
            let
                newModel =
                    setIndexDate model newIndexDate
            in
            ( { newModel
                | monthChange = Previous
              }
            , Cmd.none
            )

        GetToday today ->
            let
                updatedModel =
                    setIndexDate model today
            in
            ( { updatedModel
                | today = Just (setDayOfMonth today (Date.day today))
                , yearList = Just (defaultedYearList model.yearList today)
              }
            , Cmd.none
            )

        SetSelectionMode mode ->
            case ( mode, model.today, model.yearList ) of
                ( YearPicker, Just today, Just yearList ) ->
                    let
                        workingDate =
                            Maybe.withDefault today model.selectedDate

                        scrollId =
                            "edp-year-picker-" ++ model.id

                        selectedYearIndex =
                            List.partition (\year -> year <= Date.year workingDate) yearList
                                |> Tuple.first
                                |> List.length

                        yOffset =
                            (toFloat selectedYearIndex * 40) - (4 * 40)
                    in
                    ( { model | selectionMode = mode }
                    , Task.attempt (\_ -> NoOp) (Dom.setViewportOf scrollId 0 yOffset)
                    )

                ( Calendar, _, _ ) ->
                    ( { model | selectionMode = mode, monthChange = Next }, Cmd.none )

                ( _, _, _ ) ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| The second argument passed to `DatePicker.view`. These are configuration properties
that generally determine the range of selectable dates. Extend off `DatePicker.defaultProps`
to avo HtmlAttr.idhaving to define all of these when you only wish to configure a few.

---

**Property Descriptions**

given the year, return whether this year is allowed to be selected

    canSelectYear : Int -> Bool

given the year and the month, return whether this month is allowed to be selected

    canSelectMonth : Int -> Month -> Bool

given the date, return whether this is allowed to be selected

    canSelectDate : Date -> Bool

should the footer of the calendar with the "CANCEL" and "OK" buttons display

    hideFooter : Bool

text for the "OK" button which is enabled whenever a date is selected.
defaults to "OK"

    okButtonText : String

text for the "CANCEL" button. Defaults to "CANCEL"

    cancelButtonText : String

return whatever text to show given the month (this is just below the calendar header)

    monthDisplay : Time.Month -> String

return whatever text (generally a single letter or two) to show
given the weekday (these are the small letters the top of the month)

    daySymbol : Time.Weekday -> String

return whatever text to show given the selected Date
(This is the large display text on the calendar header)
The first date is the "selectedDate" which may not yet be defined.
The second is the "indexDate" which is the current placeholder
date being used (generally set to today's date by default)

    selectedDateDisplay : Maybe Date -> Date -> String

-}
type alias Props =
    { canSelectYear : Int -> Bool
    , canSelectMonth : Int -> Month -> Bool
    , canSelectDate : Date -> Bool
    , hideFooter : Bool
    , monthDisplay : Month -> String
    , daySymbol : Weekday -> String
    , selectedDateDisplay : Maybe Date -> Date -> String
    , okButtonText : String
    , cancelButtonText : String
    }


defaultedYearList : Maybe (List Int) -> Date -> List Int
defaultedYearList yearList indexDate =
    case yearList of
        Just list ->
            list

        Nothing ->
            List.range (Date.year indexDate - 120) (Date.year indexDate + 120)


{-| Use the default props if you don't want to support any sort of configuration.
These mostly center around limiting the user to a specific selection range of dates.
By default, nothing is restricted.

Here's an example of how you might configure these:

    getDatePickerProps : DatePicker.Props
    getDatePickerProps =
        let
            defaultProps =
                DatePicker.defaultProps
        in
        { defaultProps
            | canSelectYear = \year -> year < 2020
            , okButtonText = "CONFIRM"
        }

-}
defaultProps : Props
defaultProps =
    { canSelectYear = \year -> True
    , canSelectMonth = \year month -> True
    , canSelectDate = \date -> True
    , hideFooter = False
    , monthDisplay = monthDisplay
    , daySymbol = daySymbol
    , selectedDateDisplay =
        \maybeDate date ->
            Maybe.map getDayMonthText maybeDate
                |> Maybe.withDefault (getDayMonthText date)
    , okButtonText = "OK"
    , cancelButtonText = "CANCEL"
    }


animatedUi =
    Animated.ui
        { behindContent = Element.behindContent
        , htmlAttribute = Element.htmlAttribute
        , html = Element.html
        }


displayYear =
    Date.year >> String.fromInt >> text


headerYearDisplay : Date -> InitializedModel -> Props -> Element Msg
headerYearDisplay displayDate model props =
    el
        ([ Events.onClick (SetSelectionMode YearPicker)
         , paddingXY 10 12
         , Font.size 14
         , pointer
         , Font.semiBold
         ]
            ++ (if model.selectionMode /= YearPicker then
                    [ Border.shadow
                        { offset = ( 1, 1 )
                        , size = 1
                        , blur = 1
                        , color = rgba 0 0 0 0.18
                        }
                    , Font.color (rgba 1 1 1 0.8)
                    , Font.regular
                    ]

                else
                    []
               )
        )
        (displayYear displayDate)


headerDayMonthDisplay : Bool -> Maybe Date -> InitializedModel -> Props -> Maybe (Element Msg)
headerDayMonthDisplay isPreviousDate date model props =
    Maybe.map
        (\justDate ->
            el
                ([ Events.onClick <| SetSelectionMode Calendar
                 , Font.size 32
                 , pointer
                 , if isPreviousDate then
                    paddingXY 10 3

                   else
                    paddingXY 10 4
                 ]
                    ++ (if model.selectionMode /= Calendar then
                            [ if not isPreviousDate then
                                Border.shadow
                                    { offset = ( 2, 2 )
                                    , size = 0.5
                                    , blur = 1
                                    , color = rgba 0 0 0 0.18
                                    }

                              else
                                noAttr
                            , Font.color (rgba 1 1 1 0.8)
                            ]

                        else
                            []
                       )
                )
                (el [ centerY ] (text (props.selectedDateDisplay date model.indexDate)))
        )
        date


headerSection : Date -> InitializedModel -> Props -> Element Msg
headerSection displayDate model props =
    let
        animation =
            Animation.fromTo
                { duration = 350
                , options = [ Animation.easeOut ]
                }
                [ P.y
                    (Maybe.map (always 0) model.previousSelectedDate
                        |> Maybe.withDefault 40
                    )
                ]
                [ P.y
                    (Maybe.map (always -40) model.previousSelectedDate
                        |> Maybe.withDefault 0
                    )
                ]
    in
    column
        [ Background.color (rgb255 96 181 205)
        , width fill
        , height (px 104)
        , Font.color (rgb 1 1 1)
        , paddingXY 20 8
        , spacing 4
        ]
        [ headerYearDisplay displayDate model props
        , Keyed.el
            [ height (px 55)
            , width fill
            , clipY
            ]
            ( model.previousSelectedDate
                |> Maybe.map toRataDie
                |> Maybe.map String.fromInt
                |> Maybe.withDefault ""
            , animatedUi column
                animation
                []
                [ Maybe.withDefault
                    Element.none
                    (headerDayMonthDisplay True
                        model.previousSelectedDate
                        model
                        props
                    )
                , headerDayMonthDisplay False
                    (if isJust model.selectedDate then
                        model.selectedDate

                     else
                        Just model.today
                    )
                    model
                    props
                    |> Maybe.withDefault Element.none
                ]
            )
        ]


monthChangeSection : InitializedModel -> Props -> Element Msg
monthChangeSection model props =
    let
        year =
            Date.year model.indexDate

        previousMonthIndexDate =
            add Months -1 model.indexDate

        nextMonthIndexDate =
            add Months 1 model.indexDate

        canSelectNext =
            props.canSelectMonth year (Date.month nextMonthIndexDate)
                && props.canSelectYear (Date.year nextMonthIndexDate)

        canSelectPrevious =
            props.canSelectMonth year (Date.month previousMonthIndexDate)
                && props.canSelectYear (Date.year previousMonthIndexDate)
    in
    row
        [ paddingXY 15 11
        , width fill
        , Font.size 16
        , Font.color (rgba 0 0 0 0.84)
        ]
        [ el
            [ Events.onClick <|
                if canSelectPrevious then
                    PreviousMonth previousMonthIndexDate

                else
                    NoOp
            , alignLeft
            , Border.color (rgba 0 0 0 0.54)
            , Border.widthEach { left = 2, bottom = 2, right = 0, top = 0 }
            , rotate (pi / 4)
            , width (px 14)
            , height (px 14)
            , pointer
            ]
            Element.none
        , el [ centerX ]
            (text
                (let
                    ( monthFull, monthInt ) =
                        getMonthInfo (Date.month model.indexDate) props.monthDisplay
                 in
                 monthFull ++ " " ++ (String.fromInt <| Date.year model.indexDate)
                )
            )
        , el
            [ Events.onClick <|
                if canSelectNext then
                    NextMonth nextMonthIndexDate

                else
                    NoOp
            , alignRight
            , Border.color (rgba 0 0 0 0.54)
            , Border.widthEach { left = 2, bottom = 2, right = 0, top = 0 }
            , rotate (5 * pi / 4)
            , width (px 14)
            , height (px 14)
            , pointer
            ]
            Element.none
        ]


weekSection : InitializedModel -> Props -> Element Msg
weekSection model props =
    row
        [ centerX
        ]
        (List.map
            (\symbol ->
                el
                    [ width (px 40)
                    , height (px 40)
                    , Font.size 12
                    , Font.color (rgba 0 0 0 0.54)
                    ]
                    (el [ centerX, centerY ] (text symbol))
            )
            (List.map props.daySymbol
                [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
            )
        )


daySectionMonth : InitializedModel -> Props -> Element Msg
daySectionMonth model props =
    let
        weekRows acc days =
            case List.take 7 days of
                [] ->
                    acc

                xs ->
                    weekRows (acc ++ [ xs ]) (List.drop 7 days)
    in
    List.map
        (\( dayNum, date ) ->
            let
                isSelected =
                    case model.selectedDate of
                        Just selected ->
                            Date.toRataDie selected == Date.toRataDie date

                        Nothing ->
                            False

                isToday =
                    Date.toRataDie model.today == Date.toRataDie date

                isPlaceholder =
                    dayNum == 0

                canSelect =
                    not isPlaceholder && props.canSelectDate date
            in
            el
                ([ Events.onClick
                    (case ( canSelect, model.selectedDate ) of
                        ( True, Just previousSelected ) ->
                            if Date.toRataDie previousSelected == Date.toRataDie date then
                                NoOp

                            else
                                DateSelected date previousSelected

                        ( True, Nothing ) ->
                            DateSelected date model.today

                        ( False, _ ) ->
                            NoOp
                    )
                 , Border.rounded 50
                 , width (px 40)
                 , height (px 40)
                 , Font.size 12
                 ]
                    ++ (if isSelected then
                            [ Background.color (rgb255 96 181 205)
                            , Font.color (rgb 1 1 1)
                            , pointer
                            ]

                        else if canSelect then
                            [ mouseOver [ Background.color (rgba 0 0 0 0.08) ]
                            , pointer
                            ]
                                ++ (if isToday then
                                        [ Font.color (rgb255 96 181 205)
                                        , Font.semiBold
                                        ]

                                    else
                                        []
                                   )

                        else
                            []
                       )
                )
                (el [ centerX, centerY ]
                    (text <|
                        if isPlaceholder then
                            " "

                        else
                            String.fromInt dayNum
                    )
                )
        )
        model.currentMonthMap
        |> weekRows []
        |> List.map
            (row
                []
            )
        |> column
            [ alignTop
            , centerX
            , height fill
            ]


getMonthKey : Date -> String
getMonthKey date =
    Tuple.first <| getMonthInfo (Date.month date) monthDisplay


previousMonthBody : InitializedModel -> Props -> Maybe (Element Msg)
previousMonthBody model props =
    Maybe.map
        (\previousMonthMap ->
            el
                [ width fill ]
                (daySectionMonth { model | currentMonthMap = previousMonthMap } props)
        )
        model.previousMonthMap


calendarBody : InitializedModel -> Props -> Element Msg
calendarBody model props =
    let
        animation =
            Animation.fromTo
                { duration = 350
                , options = [ Animation.easeOut ]
                }
                [ P.x
                    (Maybe.map (always origin) model.previousMonthMap
                        |> Maybe.withDefault 0
                    )
                ]
                [ P.x
                    (Maybe.map (always destination) model.previousMonthMap
                        |> Maybe.withDefault 0
                    )
                ]

        ( origin, destination, body ) =
            if model.monthChange == Next then
                ( 0
                , -280
                , [ Maybe.withDefault Element.none
                        (previousMonthBody model props)
                  , daySectionMonth model props
                  ]
                )

            else
                ( -280
                , 0
                , [ daySectionMonth model props
                  , Maybe.withDefault Element.none
                        (previousMonthBody model props)
                  ]
                )
    in
    Keyed.el
        [ clipX
        , width fill
        , height (px 244)
        ]
        ( getMonthKey model.indexDate
        , animatedUi row
            animation
            [ Font.color (rgba 0 0 0 0.82)
            , paddingXY 10 2
            ]
            body
        )


bottomSection : InitializedModel -> Props -> Element Msg
bottomSection model props =
    let
        disableOk =
            model.selectedDate == Nothing

        buttonStyle isOk =
            [ paddingXY 16 8
            , Border.rounded 5
            , alignRight
            ]
                ++ (if isOk && disableOk then
                        [ Font.color (rgba 0 0 0 0.16)
                        , htmlAttribute <| HtmlAttr.style "cursor" "not-allowed"
                        ]

                    else
                        [ pointer
                        , mouseOver
                            [ Background.color (rgba 0 0 0 0.08) ]
                        ]
                   )
    in
    row
        [ Font.color (rgb255 96 181 205)
        , Font.size 16
        , Font.family
            [ Font.sansSerif ]
        , width fill
        , spacing 10
        , paddingEach { top = 0, left = 10, right = 10, bottom = 20 }
        , Font.bold
        , alignBottom
        ]
        [ el
            (Events.onClick CancelClicked
                :: buttonStyle False
            )
            (text props.cancelButtonText)
        , el
            (Events.onClick
                (case model.selectedDate of
                    Just date ->
                        SubmitClicked date

                    Nothing ->
                        NoOp
                )
                :: buttonStyle True
            )
            (text props.okButtonText)
        ]


yearSection : InitializedModel -> Props -> Element Msg
yearSection model props =
    let
        workingDate =
            Maybe.withDefault model.today model.selectedDate

        applyYear dt year =
            Date.fromCalendarDate
                year
                (Date.month dt)
                (Date.day dt)

        viewYear year =
            let
                canSelect =
                    props.canSelectYear year
            in
            el
                [ Events.onClick <|
                    if canSelect then
                        DateSelected (applyYear workingDate year) workingDate

                    else
                        NoOp
                , centerX
                , Font.center
                , Font.color (rgb255 96 181 205)
                , mouseOver
                    [ Background.color (rgba 0 0 0 0.08) ]
                , width (px 200)
                , Border.rounded 5
                , if year == Date.year workingDate then
                    Font.size 32

                  else
                    Font.size 16
                , Font.bold
                , Font.family
                    [ Font.sansSerif ]
                , paddingXY 0 12
                ]
                (text (String.fromInt year))
    in
    column
        [ width fill
        , height (px 320)
        , scrollbarY
        , htmlAttribute <| HtmlAttr.id <| "edp-year-picker-" ++ model.id
        ]
        (List.map viewYear model.yearList)


{-| The main view for the date picker. Use `Html.map` so the returned type doesn't conflict with
your view's type.

    import DatePicker
    ...
    view : Model -> Html.Html Msg
    view model =
        Html.map DatePickerMsg <|
            DatePicker.view
                model.datePickerData
                DatePicker.defaultProps

-}
view : Model -> Props -> Html.Html Msg
view model props =
    Element.layout [] <|
        Maybe.withDefault Element.none <|
            Maybe.map4
                (\today indexDate currentMonthMap yearList ->
                    let
                        displayDate =
                            Maybe.withDefault today model.selectedDate

                        initializedModel =
                            { id = model.id
                            , today = today
                            , indexDate = indexDate
                            , selectedDate = model.selectedDate
                            , previousSelectedDate = model.previousSelectedDate
                            , currentMonthMap = currentMonthMap
                            , previousMonthMap = model.previousMonthMap
                            , monthChange = model.monthChange
                            , selectionMode = model.selectionMode
                            , yearList = yearList
                            }

                        footer =
                            if props.hideFooter then
                                Element.none

                            else
                                bottomSection initializedModel props

                        mainSection =
                            case initializedModel.selectionMode of
                                Calendar ->
                                    column
                                        [ width fill
                                        , height fill
                                        ]
                                        [ monthChangeSection initializedModel props
                                        , weekSection initializedModel props
                                        , calendarBody initializedModel props
                                        , footer
                                        ]

                                YearPicker ->
                                    column
                                        [ width fill
                                        , height fill
                                        ]
                                        [ yearSection initializedModel props
                                        , footer
                                        ]
                    in
                    column
                        [ width (px 300)
                        , height fill
                        , Font.family
                            [ Font.typeface "Avenir"
                            , Font.sansSerif
                            ]
                        ]
                        [ headerSection displayDate initializedModel props
                        , mainSection
                        ]
                )
                model.today
                model.indexDate
                model.currentMonthMap
                model.yearList


noAttr =
    htmlAttribute <| HtmlAttr.class ""
