port module Main exposing (..)

import HttpBuilder
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto
import Time exposing (Time, minute)


kintoServer =
    "https://kinto.dev.mozaws.net/v1"


kintoBucket =
    "stepfunction"


kintoCollection =
    "manual_steps"



-- Model


type alias Url =
    String


type alias Flags =
    { email : Maybe String
    , bearer : Maybe Bearer
    , redirectUrl : String
    }


type alias Model =
    { email : Maybe String
    , bearer : Maybe Bearer
    , records : Maybe (List Record)
    , error : Maybe String
    , redirectUrl : String
    , kintoServer : String
    , kintoBucket : String
    , kintoCollection : String
    }


type alias Bearer =
    String


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
    | LoadRecords
    | FetchRecordsResponse (Result Kinto.Error (List Record))
    | Logout
    | AcceptStep String
    | RejectStep String
    | AnswerResponse (Result Kinto.Error ())
    | Tick Time



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { email = flags.email
            , bearer = flags.bearer
            , records = Nothing
            , error = Nothing
            , redirectUrl = flags.redirectUrl
            , kintoServer = kintoServer
            , kintoBucket = kintoBucket
            , kintoCollection = kintoCollection
            }
    in
        update LoadRecords model



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every minute Tick ]



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewEmail email ->
            { model | email = Just email }
                ! [ saveData { key = "email", value = Encode.string email } ]

        LoadRecords ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just _ ->
                    { model
                        | records = Just []
                        , error = Nothing
                    }
                        ! [ fetchRecordList model ]

        FetchRecordsResponse (Ok recordList) ->
            { model
                | records = Just recordList
                , error = Nothing
            }
                ! []

        FetchRecordsResponse (Err (Kinto.KintoError 403 _ error)) ->
            { model
                | records = Just []
                , error = Nothing
            }
                ! []

        FetchRecordsResponse (Err error) ->
            { model
                | error = Just <| toString error
                , records = Nothing
            }
                ! []

        Logout ->
            { model
                | error = Nothing
                , records = Nothing
                , bearer = Nothing
            }
                ! [ saveData { key = "bearer", value = Encode.null } ]

        AcceptStep recordId ->
            model ! [ answerStep model recordId "SUCCEED" ]

        RejectStep recordId ->
            model ! [ answerStep model recordId "FAIL" ]

        AnswerResponse (Err error) ->
            { model | error = Just <| toString error } ! []

        AnswerResponse (Ok _) ->
            model ! [ fetchRecordList model ]

        Tick _ ->
            model ! [ fetchRecordList model ]



-- Kinto related


answerUrl : String -> String -> String -> String -> Url
answerUrl baseUrl bucketName collectionName recordId =
    (Kinto.endpointUrl baseUrl (Kinto.RecordEndpoint bucketName collectionName recordId))
        ++ "/stepfunction"


answerResource : String -> String -> String -> Encode.Value -> Kinto.Client -> HttpBuilder.RequestBuilder ()
answerResource bucketName collectionName recordId body client =
    answerUrl client.baseUrl bucketName collectionName recordId
        |> HttpBuilder.post
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody body


encodeAnswer : String -> Encode.Value
encodeAnswer answer =
    Encode.object [ ( "answer", Encode.string answer ) ]


answerStep : Model -> RecordId -> String -> Cmd Msg
answerStep model recordId answer =
    case model.bearer of
        Nothing ->
            Cmd.none

        Just bearer ->
            let
                client =
                    Kinto.client model.kintoServer (Kinto.Custom "Portier" bearer)
            in
                client
                    |> answerResource model.kintoBucket model.kintoCollection recordId (encodeAnswer answer)
                    |> Kinto.send AnswerResponse


fetchRecordList : Model -> Cmd Msg
fetchRecordList model =
    case model.bearer of
        Nothing ->
            Cmd.none

        Just bearer ->
            let
                client =
                    Kinto.client model.kintoServer (Kinto.Custom "Portier" bearer)

                recordResource =
                    Kinto.recordResource model.kintoBucket model.kintoCollection decodeRecord
            in
                client
                    |> Kinto.getList recordResource
                    |> Kinto.sortBy [ "last_modified" ]
                    |> Kinto.send FetchRecordsResponse


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
                , Html.Attributes.method "POST"
                , Html.Attributes.action (model.kintoServer ++ "/portier/login")
                ]
                [ Html.div [ Html.Attributes.class "main-login-form" ]
                    [ Html.div [ Html.Attributes.class "login-group" ]
                        [ Html.div [ Html.Attributes.class "form-group" ]
                            [ Html.label
                                [ Html.Attributes.for "fp_email"
                                , Html.Attributes.class "sr-only"
                                ]
                                [ Html.text "Email address" ]
                            , Html.input
                                [ Html.Attributes.class "form-control"
                                , Html.Attributes.id "fp_email"
                                , Html.Attributes.placeholder "john.doe@tld.com"
                                , Html.Attributes.value (Maybe.withDefault "" model.email)
                                , Html.Attributes.type_ "email"
                                , Html.Attributes.name "email"
                                , Html.Events.onInput NewEmail
                                ]
                                []
                            , Html.input
                                [ Html.Attributes.name "redirect"
                                , Html.Attributes.value model.redirectUrl
                                , Html.Attributes.type_ "hidden"
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
            case records of
                [] ->
                    Html.h4 [] [ Html.text "No step function needing your attention." ]

                _ ->
                    Html.table [ Html.Attributes.class "table" ]
                        [ Html.thead [ Html.Attributes.class "thead-inverse" ]
                            [ Html.tr []
                                [ Html.th [] [ Html.text "Subject" ]
                                , Html.th [] [ Html.text "Status" ]
                                , Html.th [] [ Html.text "Actions" ]
                                ]
                            ]
                        , Html.tbody [] <| List.map (displayRecord model) records
                        ]

        Just error ->
            Html.pre [] [ Html.text error ]


recordTitle : Record -> String
recordTitle record =
    "State Machine:\n  " ++ record.stateMachineArn ++ "\n\nActivity:\n  " ++ record.activityArn


displayRecord : Model -> Record -> Html.Html Msg
displayRecord model record =
    Html.tr []
        [ Html.td
            [ Html.Attributes.title (recordTitle record) ]
            [ Html.text (Maybe.withDefault record.id record.subject) ]
        , Html.td [] [ Html.text (Maybe.withDefault "UNANSWERED" record.status) ]
        , Html.td [] <|
            case record.status of
                Nothing ->
                    [ Html.div [ Html.Attributes.class "btn-group" ]
                        [ Html.button
                            [ Html.Attributes.class "btn btn-success"
                            , Html.Events.onClick (AcceptStep record.id)
                            ]
                            [ Html.text "Accept" ]
                        , Html.button
                            [ Html.Attributes.class "btn btn-danger"
                            , Html.Events.onClick (RejectStep record.id)
                            ]
                            [ Html.text "Reject" ]
                        ]
                    ]

                Just status ->
                    []
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



-- Save data


port saveData : { key : String, value : Encode.Value } -> Cmd msg
