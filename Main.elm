module Main exposing (..)

import Html


-- Model


type alias Model =
    { email : String }



-- Msg


type Msg
    = NewEmail String



-- Init


init : ( Model, Cmd Msg )
init =
    { email = "" } ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewEmail email ->
            { model | email = email } ! []



-- View


view : Model -> Html.Html Msg
view model =
    Html.div [] [ Html.text "Manual StepFunction Dashboard" ]



-- Main


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
