port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr exposing (attribute, class, classList, style)
import Html.Events exposing (onClick, onInput)
import Http
import List.Extra as List
import Subtitles exposing (Subtitle, decodeSubtitles)


backendUrlRoot : String
backendUrlRoot =
    "http://127.0.0.1:8080/static/"


type alias TabData =
    { text : String
    , icon : String
    , content : Model -> Html Msg
    }


findTabId : TabId
findTabId =
    0


listenTabId : TabId
listenTabId =
    1


tabs : List TabData
tabs =
    [ { text = "Find"
      , icon = "search"
      , content = viewFindTab
      }
    , { text = "Listen"
      , icon = "headphones"
      , content = viewListenTab
      }
    , { text = "Review"
      , icon = "grading"
      , content = viewReviewTab
      }
    ]


viewFindTab : Model -> Html Msg
viewFindTab model =
    Html.div []
        (List.map (viewVideoCard model) model.videos)


viewListenTab : Model -> Html Msg
viewListenTab model =
    case getVideo model.videoId model.videos of
        Nothing ->
            Html.div [ class "flex flex-col items-center" ]
                [ Html.div [ class "mb-2 text-xl" ] [ Html.text "No video selected" ]
                , Html.button [ onClick (TabClicked findTabId) ]
                    [ Html.text "Find a video" ]
                ]

        Just video ->
            Html.div [ class "flex flex-col items-center gap-2" ]
                [ Html.div [ class "text-xl text-center" ]
                    [ Html.text video.title ]
                , Html.div [ class "w-full" ]
                    [ Html.input
                        [ Attr.type_ "range"
                        , Attr.min "0"
                        , Attr.max (String.fromFloat video.duration)
                        , Attr.step "1"
                        , Attr.value (String.fromInt (round model.videoTime))
                        , onInput (String.toFloat >> Maybe.map SetVideoTime >> Maybe.withDefault (SetVideoTime 0))
                        ]
                        []
                    ]
                , Html.div []
                    [ Html.text
                        (formatTime model.videoTime
                            ++ " / "
                            ++ formatTime video.duration
                        )
                    ]
                , Html.div [ class "flex gap-2" ]
                    [ Html.button
                        [ onClick FastRewind
                        , attribute "aria-label" "Rewind"
                        ]
                        [ Html.text "<<" ]
                    , playButton model
                    , Html.button
                        [ onClick FastForward
                        , attribute "aria-label" "Fast-forward"
                        ]
                        [ Html.text ">>" ]
                    ]
                , Html.div []
                    [ Html.button [ onClick SaveRecording ] [ Html.text "Save" ] ]
                , Html.div []
                    (video.subtitles
                        |> List.map
                            (\subtitle -> Html.div [ class "text-center" ] [ Html.text subtitle.text ])
                    )
                ]


viewReviewTab : Model -> Html Msg
viewReviewTab model =
    if List.isEmpty model.recordings then
        Html.div [ class "text-center text-xl" ] [ Html.text "No recordings saved yet" ]

    else
        Html.div []
            (List.map
                (\recording ->
                    Html.div []
                        [ Html.div []
                            [ getVideo (Just recording.videoId) model.videos
                                |> Maybe.map .title
                                |> Maybe.withDefault ""
                                |> Html.text
                            ]
                        , Html.div [] [ Html.text (formatTime recording.time) ]
                        , Html.div []
                            [ if Just recording.videoId == model.videoId then
                                Html.button [ onClick (PlayRecording recording) ]
                                    [ Html.text "Play recording" ]

                              else
                                Html.button [ onClick (LoadVideo recording.videoId) ]
                                    [ Html.text "Load video" ]
                            ]
                        ]
                )
                model.recordings
            )


formatTime : Float -> String
formatTime totalSeconds =
    let
        seconds =
            floor totalSeconds |> modBy 60

        minutes =
            (floor totalSeconds // 60) |> modBy 60

        hours =
            floor totalSeconds // 60 // 60
    in
    [ hours, minutes, seconds ]
        |> List.map (String.fromInt >> String.padLeft 2 '0' >> String.right 2)
        |> String.join ":"


type alias Model =
    { tabId : TabId
    , videoId : Maybe VideoId
    , videoIsPlaying : Bool
    , videoTime : VideoTime
    , videos : List Video
    , recordings : List Recording
    }


type alias TabId =
    Int


type alias VideoId =
    String


type alias VideoTime =
    Float


type alias Recording =
    { videoId : VideoId
    , time : Float
    }


type alias Video =
    { id : VideoId
    , title : String
    , duration : Float
    , subtitles : List Subtitle
    }


getVideo : Maybe VideoId -> List Video -> Maybe Video
getVideo videoId videos =
    videoId |> Maybe.andThen (\id -> List.find (.id >> (==) id) videos)


fetchSubtitles : String -> Cmd Msg
fetchSubtitles videoId =
    Http.get
        { url = backendUrlRoot ++ videoId ++ ".json"
        , expect = Http.expectJson GotSubtitles (decodeSubtitles videoId)
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tabId = 0
      , videoId = Nothing
      , videoIsPlaying = False
      , videoTime = 0
      , videos = []
      , recordings = []
      }
    , fetchSubtitles "rg3JqmUmzlE"
    )


type Msg
    = TabClicked TabId
    | ListenToVideo VideoId
    | PlayVideo
    | PauseVideo
    | FastForward
    | FastRewind
    | GetVideoTime VideoTime
    | SetVideoTime VideoTime
    | SaveRecording
    | PlayRecording Recording
    | LoadVideo VideoId
    | GotSubtitles (Result Http.Error (List Subtitle))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked tabId ->
            ( { model | tabId = tabId }, Cmd.none )

        ListenToVideo videoId ->
            ( { model
                | tabId = listenTabId
                , videoId = Just videoId
                , videoIsPlaying = True
              }
            , if Just videoId == model.videoId then
                playVideo ()

              else
                startVideo videoId
            )

        PlayVideo ->
            ( { model | videoIsPlaying = True }, playVideo () )

        PauseVideo ->
            ( { model | videoIsPlaying = False }, pauseVideo () )

        FastForward ->
            ( model, fastForward () )

        FastRewind ->
            ( model, fastRewind () )

        GetVideoTime videoTime ->
            ( { model | videoTime = videoTime }, Cmd.none )

        SetVideoTime videoTime ->
            ( model, setVideoTime videoTime )

        SaveRecording ->
            case model.videoId of
                Nothing ->
                    ( model, Cmd.none )

                Just videoId ->
                    ( { model
                        | recordings =
                            model.recordings
                                ++ [ { videoId = videoId, time = model.videoTime } ]
                      }
                    , Cmd.none
                    )

        PlayRecording recording ->
            ( { model | videoIsPlaying = True }, playRecording recording )

        LoadVideo videoId ->
            ( { model
                | videoId = Just videoId
                , videoIsPlaying = False
                , videoTime = 0
              }
            , loadVideo videoId
            )

        GotSubtitles response ->
            case response of
                Err err ->
                    let
                        _ =
                            Debug.log "GotSubtitles error" err
                    in
                    ( model, Cmd.none )

                Ok subtitles ->
                    let
                        videoId =
                            List.head subtitles
                                |> Maybe.map .videoId
                                |> Maybe.withDefault ""

                        maybeVideo =
                            List.find (.id >> (==) videoId) model.videos
                    in
                    case maybeVideo of
                        Nothing ->
                            ( { model
                                | videos =
                                    Debug.log "New videos" <|
                                        model.videos
                                            ++ [ { id = videoId
                                                 , title = "Test title"
                                                 , duration = 1234
                                                 , subtitles = subtitles
                                                 }
                                               ]
                              }
                            , Cmd.none
                            )

                        Just _ ->
                            ( { model
                                | videos =
                                    List.map (\video -> { video | subtitles = subtitles })
                                        model.videos
                              }
                            , Cmd.none
                            )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewTabs model ]
        , Html.div [ class "pt-24 px-3" ]
            [ tabs
                |> List.getAt model.tabId
                |> Maybe.map (\tab -> tab.content model)
                |> Maybe.withDefault (Html.text "")
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    Html.div [ class "h-16 flex text-xl bg-black" ]
        (List.indexedMap (viewTab model) tabs)


viewTab : Model -> TabId -> TabData -> Html Msg
viewTab model tabId tab =
    Html.button
        [ classList [ ( "text-cyan-400 border-b-2 border-cyan-400", model.tabId == tabId ) ]
        , class "grow h-full flex justify-center items-center cursor-pointer"
        , onClick (TabClicked tabId)
        ]
        [ Html.text tab.text ]


viewVideoCard : Model -> Video -> Html Msg
viewVideoCard model video =
    Html.div [ class "px-4 pt-4 mb-4" ]
        [ Html.div []
            [ Html.h2 [ class "text-xl" ] [ Html.text video.title ]
            , Html.button [ onClick (ListenToVideo video.id) ]
                [ Html.text "Listen" ]
            , if model.videoId == Just video.id then
                playButton model

              else
                Html.text ""
            ]
        ]


playButton : Model -> Html Msg
playButton model =
    if model.videoIsPlaying then
        Html.button
            [ onClick PauseVideo
            , attribute "aria-label" "Pause"
            ]
            [ Html.text "||" ]

    else
        Html.button
            [ onClick PlayVideo
            , attribute "aria-label" "Play"
            ]
            [ Html.text "▶" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    getVideoTime GetVideoTime


port startVideo : String -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port fastForward : () -> Cmd msg


port fastRewind : () -> Cmd msg


port getVideoTime : (Float -> msg) -> Sub msg


port setVideoTime : Float -> Cmd msg


port loadVideo : String -> Cmd msg


port playRecording : Recording -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
