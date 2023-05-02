module ElmHledgerWeb exposing (main)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, layout, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Http
import Json.Decode as D exposing (Decoder)



--
-- TYPES
--


type alias Model =
    { accNames : List String
    , transactions : List Transaction
    }


type alias Transaction =
    { description : String
    , date : String
    , index : Int
    , postings : List Posting
    }


type alias Posting =
    { account : String
    , commodity : String
    , amount : Float
    }


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
        (D.field "tdate" D.string)
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
                    ( { model | transactions = [ Transaction "Something went wrong..." "" 0 [] ] }, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layout [] <|
        column [ width fill ]
            [ button
                [ Background.color (rgb255 89 205 255)
                , Border.rounded 5
                , width <| px 150
                , height <| px 70
                , Font.size 15
                ]
                { onPress = Just GetTransactions
                , label = el [ centerX, centerY ] (text "Get Transactions")
                }
            , column
                [ spacing 10
                , centerX
                ]
                (List.map txnView model.transactions)
            ]


txnView : Transaction -> Element msg
txnView txn =
    el
        [ Border.width 1
        , Border.rounded 5
        , Background.color (rgb255 100 100 100)
        , padding 20
        , width <| px 600
        ]
        (row []
            [ dateView txn.date
            , text txn.description
            ]
        )


dateView : String -> Element msg
dateView date =
    el
        [ Border.width 1
        , width <| px 150
        , height <| px 50
        , Border.rounded 5
        , Background.color (rgb255 150 150 150)
        ]
        (el
            [ centerX, centerY ]
            (text date)
        )



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
    ( initModel, Cmd.none )


initModel =
    { accNames = []
    , transactions = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
