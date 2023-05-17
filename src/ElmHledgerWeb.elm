module ElmHledgerWeb exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Http
import Iso8601
import Json.Decode as D exposing (Decoder)
import Round
import Time



--
-- TYPES
--


type alias Model =
    { accNames : List String
    , transactions : List Transaction
    }


type alias Transaction =
    { description : String
    , date : Time.Posix
    , index : Int
    , postings : List Posting
    }


type alias Posting =
    { account : String
    , commodity : String
    , amount : Float
    }


type alias AccSummary =
    Dict String Float


type Msg
    = GetAccNames
    | GetTransactions
    | GotAccNames (Result Http.Error (List String))
    | GotTransactions (Result Http.Error (List Transaction))



--
-- JSON DECODERS
--


getTransactions : Cmd Msg
getTransactions =
    Http.get
        { url = "http://127.0.0.1:5000/transactions"
        , expect = Http.expectJson GotTransactions (D.list txnDecoder)
        }


txnDecoder : Decoder Transaction
txnDecoder =
    D.map4 Transaction
        (D.field "tdescription" D.string)
        (D.field "tdate" dateDecoder)
        (D.field "tindex" D.int)
        (D.field "tpostings" (D.list postingDecoder))


postingDecoder : Decoder Posting
postingDecoder =
    D.map3 Posting
        (D.field "paccount" D.string)
        (D.field "pamount" (D.index 0 (D.field "acommodity" D.string)))
        (D.field "pamount" (D.index 0 (D.at [ "aquantity", "floatingPoint" ] D.float)))


getAccNames : Cmd Msg
getAccNames =
    Http.get
        { url = "http://127.0.0.1:5000/accountnames"
        , expect = Http.expectJson GotAccNames accNamesDecoder
        }


accNamesDecoder : Decoder (List String)
accNamesDecoder =
    D.list D.string


dateDecoder : Decoder Time.Posix
dateDecoder =
    D.string
        |> D.andThen
            (\dateAsString ->
                case Iso8601.toTime dateAsString of
                    Err _ ->
                        -- For now just zero out the date if there is a problem parsing it
                        D.succeed (Time.millisToPosix 0)

                    Ok date ->
                        D.succeed date
            )



--
-- UPDATE
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAccNames ->
            ( model, getAccNames )

        GetTransactions ->
            ( model, getTransactions )

        GotAccNames accNamesResult ->
            case accNamesResult of
                Ok accNames ->
                    ( { model | accNames = accNames }, Cmd.none )

                Err _ ->
                    ( { model | accNames = [ "HTTP Error!" ] }, Cmd.none )

        GotTransactions transactionsResult ->
            case transactionsResult of
                Ok transactions ->
                    ( { model | transactions = transactions }, Cmd.none )

                Err _ ->
                    ( { model | transactions = [ Transaction "Something went wrong..." (Time.millisToPosix 0) 0 [] ] }, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layout [] <|
        column [ width fill ]
            [ column
                [ spacing 10
                , centerX
                ]
                (List.append
                    [ summaryView (getAccSummary model.transactions) ]
                    (List.map txnView model.transactions)
                )
            ]


txnView : Transaction -> Element msg
txnView txn =
    el
        [ Border.width 1
        , Border.rounded 5
        , Background.color (rgb255 100 34 214)
        , padding 10
        , width <| px 600
        ]
        (row [ spacing 10 ]
            [ dateView txn.date
            , column [ spacing 10 ]
                [ el [ Font.color (rgb255 255 255 255) ] (text txn.description)
                , postingsView txn.postings
                ]
            ]
        )


dateView : Time.Posix -> Element msg
dateView date =
    el
        [ Border.width 1
        , width <| px 60
        , height <| px 60
        , Border.rounded 5
        , Background.color (rgb255 150 150 150)
        ]
        (column [ width fill, height fill ]
            [ el
                [ centerX, centerY, Font.size 10 ]
                (text <| monthAsString <| Time.toMonth Time.utc date)
            , el
                [ centerX, centerY, Font.size 25 ]
                (text <| String.fromInt <| Time.toDay Time.utc date)
            , el
                [ centerX, centerY, Font.size 10 ]
                (text <| String.fromInt <| Time.toYear Time.utc date)
            ]
        )


postingsView : List Posting -> Element msg
postingsView postings =
    row [ width fill ]
        [ column []
            (List.map (\p -> el [ Font.size 15, Font.color (rgb255 255 255 2555) ] (text p.account)) postings)
        , column [ width fill ]
            (List.map (\p -> el [ alignRight, Font.size 15, Font.color (rgb255 255 255 2555) ] (text <| String.fromFloat <| p.amount)) postings)
        ]


monthAsString : Time.Month -> String
monthAsString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


summaryView : AccSummary -> Element msg
summaryView accSum =
    let
        accNames =
            Dict.keys accSum

        getAmount accName =
            Dict.get accName accSum
                |> Maybe.withDefault 0
                |> Round.round 2
    in
    el []
        (column []
            (List.map (\accName -> el [] <| text (accName ++ ": " ++ getAmount accName)) accNames)
        )


getAccSummary : List Transaction -> AccSummary
getAccSummary txns =
    let
        allPostings : List Posting
        allPostings =
            List.map (\t -> t.postings) txns
                |> List.concat
                -- Convert all accounts name to just the top level account
                |> List.map (\p -> { p | account = getTopLevelAccName p.account })

        topLevelAccNames =
            unique <| List.map (\p -> getTopLevelAccName p.account) allPostings

        accTotal : String -> ( String, Float )
        accTotal accName =
            List.filter (\p -> p.account == accName) allPostings
                |> List.map (\p -> p.amount)
                |> List.sum
                |> Tuple.pair accName
    in
    Dict.fromList <|
        List.map accTotal topLevelAccNames


unique : List a -> List a
unique list =
    List.foldl
        (\a uniques ->
            if List.member a uniques then
                uniques

            else
                uniques ++ [ a ]
        )
        []
        list


getTopLevelAccName : String -> String
getTopLevelAccName fullAccName =
    Maybe.withDefault "unknown" <| List.head <| String.split ":" fullAccName



--
-- MAIN
--


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, getTransactions )


initModel : { accNames : List a, transactions : List b }
initModel =
    { accNames = []
    , transactions = []
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
