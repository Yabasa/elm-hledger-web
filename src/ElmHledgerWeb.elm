module ElmHledgerWeb exposing (main)

import Browser
import Html exposing (..)
import Http
import Json.Decode exposing (Decoder, list, string)


type alias Model =
    List String


type Msg
    = GotAccNames (Result Http.Error (List String))


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
    ( [ "nothing" ], getAccountNames )


accNamesDecoder : Decoder (List String)
accNamesDecoder =
    list string


getAccountNames : Cmd Msg
getAccountNames =
    Http.get
        { url = "http://127.0.0.1:5000/accountnames"
        , expect = Http.expectJson GotAccNames accNamesDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAccNames accNamesResult ->
            case accNamesResult of
                Ok accNames ->
                    ( accNames, Cmd.none )

                Err _ ->
                    ( [ "HTTP Error!" ], Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] (List.map (\acc -> div [] [ text acc ]) model)
