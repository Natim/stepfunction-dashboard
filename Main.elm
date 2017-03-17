port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events


-- Model


type alias Record =
    { subject : String
    , activityArn : String
    , status : String
    , stateMachineArn : String
    }


type alias Model =
    { email : String
    , bearer : Maybe String
    , records : Maybe (List Record)
    }



-- Msg


type Msg
    = NewEmail String
    | Authenticate
    | BearerTokenRetrieved String



-- Init


init : ( Model, Cmd Msg )
init =
    { email = ""
    , bearer = Nothing
    , records = Nothing
    }
        ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authenticated BearerTokenRetrieved
        ]



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewEmail email ->
            { model | email = email } ! []

        Authenticate ->
            model ! []

        BearerTokenRetrieved bearer ->
            { model | bearer = Just bearer } ! []



-- View


formView : Model -> Html.Html Msg
formView model =
    Html.div
        [ Html.Attributes.class "text-center" ]
        [ Html.div [ Html.Attributes.class "logo" ] [ Html.text "sign-in" ]
        , Html.div [ Html.Attributes.class "login-form" ]
            [ Html.form
                [ Html.Attributes.id "authenticate-form"
                , Html.Attributes.class "text-left"
                , Html.Events.onSubmit Authenticate
                ]
                [ Html.div [ Html.Attributes.class "main-login-form" ]
                    [ Html.div [ Html.Attributes.class "login-group" ]
                        [ Html.div [ Html.Attributes.class "form-group" ]
                            [ Html.label [ Html.Attributes.for "fp_email", Html.Attributes.class "sr-only" ] [ Html.text "Email address" ]
                            , Html.input
                                [ Html.Attributes.class "form-control"
                                , Html.Attributes.id "fp_email"
                                , Html.Attributes.placeholder "john.doe@tld.com"
                                , Html.Events.onInput NewEmail
                                ]
                                []
                            ]
                        ]
                    , Html.button
                        [ Html.Attributes.class "login-button"
                        ]
                        [ Html.i [ Html.Attributes.class "fa fa-chevron-right" ] [] ]
                    ]
                ]
            ]
        ]


displayRecords : Model -> List Record -> Html.Html Msg
displayRecords model records =
    Html.text "Authenticated"


view : Model -> Html.Html Msg
view model =
    case model.bearer of
        Nothing ->
            formView model

        Just bearer ->
            case model.records of
                Nothing ->
                    Html.text "Authenticated, loading records..."

                Just records ->
                    displayRecords model records



-- Main


main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Oauth Flow


port authenticate : String -> Cmd msg


port authenticated : (String -> msg) -> Sub msg
