port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Encode as Encode


-- Model


type alias Flags =
    { email : Maybe String
    , bearer : Maybe String
    }


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    { email = Maybe.withDefault "" flags.email
    , bearer = flags.bearer
    , records = Nothing
    }
        ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewEmail email ->
            { model | email = email } ! [ saveData { key = "email", value = Encode.string email } ]

        Authenticate ->
            model ! [ authenticate model.email ]

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
                                , Html.Attributes.value model.email
                                , Html.Events.onInput NewEmail
                                ]
                                []
                            ]
                        ]
                    , Html.button
                        [ Html.Attributes.class "login-button"
                        , Html.Attributes.type_ "submit"
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
    Html.programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Oauth Flow


port authenticate : String -> Cmd msg



-- Save data


port saveData : { key : String, value : Encode.Value } -> Cmd msg
