port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto


-- Settings
-- If you change the kintoServer there you want to change it in ports.js too.


kintoServer =
    "https://kinto.dev.mozaws.net/v1/"


bucket =
    "stepfunction"


collection =
    "manual_steps"



-- Model


type alias Flags =
    { email : Maybe String
    , bearer : Bearer
    }


type alias Model =
    { email : String
    , bearer : Bearer
    , records : Maybe (List Record)
    , error : Maybe String
    }


type alias Bearer =
    Maybe String


type alias RecordId =
    String


type alias Record =
    { id : RecordId
    , last_modified : Int
    , subject : Maybe String
    , stateMachineArn : String
    , activityArn : String
    , status : Maybe String
    }



-- Msg


type Msg
    = NewEmail String
    | Authenticate
    | LoadRecords
    | FetchRecordsResponse (Result Kinto.Error (List Record))
    | Logout



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { email = Maybe.withDefault "" flags.email
            , bearer = flags.bearer
            , records = Nothing
            , error = Nothing
            }
    in
        update LoadRecords model



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

        LoadRecords ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just bearer ->
                    { model | records = Just [], error = Nothing } ! [ fetchRecordList bearer ]

        FetchRecordsResponse (Ok recordList) ->
            { model
                | records = Just recordList
                , error = Nothing
            }
                ! []

        FetchRecordsResponse (Err error) ->
            { model | error = Just <| toString error } ! []

        Logout ->
            { model
                | error = Nothing
                , records = Nothing
                , bearer = Nothing
            }
                ! [ saveData { key = "bearer", value = Encode.null } ]



-- Kinto related


fetchRecordList : String -> Cmd Msg
fetchRecordList bearer =
    let
        client =
            Kinto.client kintoServer (Kinto.Custom "Portier" bearer)
    in
        client
            |> Kinto.getList recordResource
            |> Kinto.sortBy [ "last_modified" ]
            |> Kinto.send FetchRecordsResponse


recordResource : Kinto.Resource Record
recordResource =
    Kinto.recordResource bucket collection decodeRecord


decodeRecord : Decode.Decoder Record
decodeRecord =
    (Decode.map6 Record
        (Decode.field "id" Decode.string)
        (Decode.field "last_modified" Decode.int)
        (Decode.maybe (Decode.field "subject" Decode.string))
        (Decode.field "stateMachineArn" Decode.string)
        (Decode.field "activityArn" Decode.string)
        (Decode.maybe (Decode.field "status" Decode.string))
    )



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
    case model.error of
        Nothing ->
            Html.table [ Html.Attributes.class "table" ]
                [ Html.thead [ Html.Attributes.class "thead-inverse" ]
                    [ Html.tr []
                        [ Html.th [] [ Html.text "Subject" ]
                        , Html.th [] [ Html.text "Status" ]
                          -- Maybe.withDefault "UNANSWERED" Html.status]
                        ]
                    ]
                , Html.tbody [] <| List.map (displayRecord model) records
                ]

        Just error ->
            Html.pre [] [ Html.text error ]


displayRecord : Model -> Record -> Html.Html Msg
displayRecord model record =
    Html.tr []
        [ Html.td [] [ Html.text (Maybe.withDefault record.id record.subject) ]
        , Html.td [] [ Html.text (String.toUpper (Maybe.withDefault "UNANSWERED" record.status)) ]
        ]


view : Model -> Html.Html Msg
view model =
    case model.bearer of
        Nothing ->
            formView model

        Just bearer ->
            Html.div []
                [ Html.h1 [] [ Html.text "Manual StepFunction Dashboard" ]
                , case model.records of
                    Nothing ->
                        Html.text "Authenticated, loading records..."

                    Just records ->
                        displayRecords model records
                , Html.a
                    [ Html.Attributes.href "#"
                    , Html.Events.onClick Logout
                    ]
                    [ Html.text "Logout" ]
                ]



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
