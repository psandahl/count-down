module Main exposing (main)

{-| foo
-}

import Html exposing (Html)
import Types exposing (Model, Msg)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { dummy = 1 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    Html.text "hej"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
