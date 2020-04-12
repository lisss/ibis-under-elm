module Main exposing (main)

import Bootstrap.CDN as CDN
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import ItemList as L


type Msg
    = Items L.Msg


type alias Model =
    { items : L.Model }


initialModel : Model
initialModel =
    { items = L.initialModel }


view : Model -> Html Msg
view model =
    div []
        [ CDN.stylesheet
        , Html.map Items <| L.view model.items
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Items x ->
            let
                ( iModel, iMsg ) =
                    L.update x model.items
            in
            ( { model | items = iModel }, Cmd.map Items iMsg )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.map Items <| L.modalSub model.items
        }
