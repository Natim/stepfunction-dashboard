port module Main exposing (..)

import HttpBuilder
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Kinto


-- Model


type alias Url =
    String


type alias Flags =
    { email : Maybe String
    , bearer : Maybe Bearer
    , kintoServer : String
    , kintoBucket : String
    , kintoCollection : String
    }


type alias Model =
    { email : String
    , bearer : Maybe Bearer
    , records : Maybe (List Record)
    , error : Maybe String
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
    | Authenticate
    | LoadRecords
    | FetchRecordsResponse (Result Kinto.Error (List Record))
    | Logout
    | AcceptStep String
    | RejectStep String
    | AnswerResponse (Result Kinto.Error ())



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { email = Maybe.withDefault "" flags.email
            , bearer = flags.bearer
            , records = Nothing
            , error = Nothing
            , kintoServer = flags.kintoServer
            , kintoBucket = flags.kintoBucket
            , kintoCollection = flags.kintoCollection
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
            { model | email = email }
                ! [ saveData { key = "email", value = Encode.string email } ]

        Authenticate ->
            model ! [ authenticate model.email ]

        LoadRecords ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just bearer ->
                    { model
                        | records = Just []
                        , error = Nothing
                    }
                        ! [ fetchRecordList model bearer ]

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

        AcceptStep recordId ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just bearer ->
                    model ! [ answerStep model bearer recordId "succeed" ]

        RejectStep recordId ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just bearer ->
                    model ! [ answerStep model bearer recordId "fail" ]

        AnswerResponse (Err error) ->
            { model | error = Just <| toString error } ! []

        AnswerResponse (Ok _) ->
            case model.bearer of
                Nothing ->
                    model ! []

                Just bearer ->
                    model ! [ fetchRecordList model bearer ]



-- Kinto related


answerUrl : String -> String -> String -> String -> Url
answerUrl baseUrl bucketName collectionName recordId =
    let
        url =
            if String.endsWith "/" baseUrl then
                String.dropRight 1 baseUrl
            else
                baseUrl

        joinUrl =
            String.join "/"
    in
        joinUrl
            [ url
            , "buckets"
            , bucketName
            , "collections"
            , collectionName
            , "records"
            , recordId
            , "stepfunction"
            ]


answerResource : String -> String -> String -> Encode.Value -> Kinto.Client -> HttpBuilder.RequestBuilder ()
answerResource bucketName collectionName recordId body client =
    answerUrl client.baseUrl bucketName collectionName recordId
        |> HttpBuilder.post
        |> HttpBuilder.withHeaders client.headers
        |> HttpBuilder.withJsonBody body


encodeAnswer : String -> Encode.Value
encodeAnswer answer =
    Encode.object [ ( "answer", Encode.string answer ) ]


answerStep : Model -> Bearer -> RecordId -> String -> Cmd Msg
answerStep model bearer recordId answer =
    let
        client =
            Kinto.client model.kintoServer (Kinto.Custom "Portier" bearer)
    in
        client
            |> answerResource model.kintoBucket model.kintoCollection recordId (encodeAnswer answer)
            |> Kinto.send AnswerResponse


fetchRecordList : Model -> Bearer -> Cmd Msg
fetchRecordList model bearer =
    let
        client =
            Kinto.client model.kintoServer (Kinto.Custom "Portier" bearer)
    in
        client
            |> Kinto.getList (recordResource model.kintoBucket model.kintoCollection)
            |> Kinto.sortBy [ "last_modified" ]
            |> Kinto.send FetchRecordsResponse


recordResource : String -> String -> Kinto.Resource Record
recordResource bucketName collectionName =
    Kinto.recordResource bucketName collectionName decodeRecord


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
                            [ Html.label
                                [ Html.Attributes.for "fp_email"
                                , Html.Attributes.class "sr-only"
                                ]
                                [ Html.text "Email address" ]
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
                        , Html.th [] [ Html.text "Actions" ]
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
        , Html.td []
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
