module Main exposing (Message, Model, Msg(..), dateFormatter, init, main, update, view)

import Browser
import Css exposing (..)
import DateFormat
import Debug
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, string)
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (Posix, Zone, utc)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Talk with Azar Darr!", body = [ model |> view |> toUnstyled ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { channel : WebData Channel, currentUsername : String, currentMessage : String }


type alias Channel =
    { name : String, messages : List Message }


type alias Message =
    { user : String, message : String, timestamp : Time.Posix }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { channel = Loading, currentMessage = "", currentUsername = "" }
    , getChannel "general"
    )


getChannel : String -> Cmd Msg
getChannel channel =
    Http.get
        { url = "https://replyr.herokuapp.com/chat/channel/" ++ channel
        , expect = Http.expectJson (RemoteData.fromResult >> ChannelResponse) decodeChannel
        }


decodeChannel : Decoder Channel
decodeChannel =
    map2 Channel (field "name" string) (field "messages" (list decodeMessage))


decodeMessage : Decoder Message
decodeMessage =
    map3 Message (field "username" string) (field "text" string) (Json.Decode.map ((\x -> x * 1000) >> Time.millisToPosix) (field "timestamp" int))



-- Update


type Msg
    = ChannelResponse (WebData Channel)
    | UpdateMessage String
    | UpdateUsername String
    | SendMessage
    | BodyResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChannelResponse channel ->
            ( { model | channel = channel }, Cmd.none )

        UpdateMessage message ->
            ( { model | currentMessage = message }, Cmd.none )

        UpdateUsername username ->
            ( { model | currentUsername = username }, Cmd.none )

        BodyResponse _ ->
            ( model, Cmd.none )

        SendMessage ->
            ( { model | currentMessage = "" }, sendMessage "general" model.currentUsername model.currentMessage )


sendMessage : String -> String -> String -> Cmd Msg
sendMessage channel username message =
    Http.post
        { url = "https://replyr.herokuapp.com/chat/channel/" ++ channel ++ "/message"
        , body = Http.jsonBody (Encode.object [ ( "username", Encode.string username ), ( "text", Encode.string message ) ])
        , expect = Http.expectString BodyResponse
        }



-- View


view : Model -> Html Msg
view model =
    case model.channel of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading."

        Failure err ->
            text ("Error: " ++ Debug.toString err)

        Success channel ->
            div [ css [ displayFlex, flexDirection column ] ]
                [ viewChannelName channel.name
                , viewMessages channel.messages
                , viewSendBox model.currentUsername model.currentMessage
                ]


viewChannelName : String -> Html Msg
viewChannelName name =
    header [ css [ fontFamilies [ "Consolas" ] ] ] [ text ("#" ++ name) ]


viewMessages : List Message -> Html Msg
viewMessages messages =
    div []
        (List.map
            (\message ->
                div []
                    [ text ("[" ++ dateFormatter utc message.timestamp ++ "] " ++ message.user ++ ": " ++ message.message)
                    ]
            )
            messages
        )


viewSendBox : String -> String -> Html Msg
viewSendBox currentUsername currentMessage =
    div []
        [ input [ placeholder "Username", onSubmit SendMessage, onInput UpdateUsername, value currentUsername ] []
        , input
            [ placeholder "Tell Azar something", autofocus True, onSubmit SendMessage, onInput UpdateMessage, value currentMessage ]
            []
        , button [ onClick SendMessage ] [text "send"]
        ]


dateFormatter : Zone -> Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        , DateFormat.text " "
        , DateFormat.hourFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        , DateFormat.text "."
        , DateFormat.millisecondFixed
        ]
