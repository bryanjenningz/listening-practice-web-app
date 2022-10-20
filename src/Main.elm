port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import List.Extra as List
import Material.Button as Button
import Material.Card as Card
import Material.Fab as Fab
import Material.IconButton as IconButton
import Material.Slider as Slider
import Material.Theme as Theme
import Random
import Subtitles exposing (Subtitle, SubtitleId, subtitles1, subtitles2, subtitles3)


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
        (List.map (viewVideoCard model)
            (List.filterMap
                (\videoId -> getVideo (Just videoId) model.videos)
                model.videosOrdered
            )
        )


viewListenTab : Model -> Html Msg
viewListenTab model =
    case getVideo model.videoId model.videos of
        Nothing ->
            Html.div [ class "flex flex-col items-center" ]
                [ Html.div [ class "mb-2 text-xl" ] [ Html.text "No video selected" ]
                , Button.raised
                    (Button.config
                        |> Button.setIcon (Just (Button.icon "search"))
                        |> Button.setOnClick (TabClicked findTabId)
                    )
                    "Find a video"
                ]

        Just video ->
            Html.div [ class "flex flex-col items-center gap-2" ]
                [ Html.div [ class "text-xl text-center" ]
                    [ Html.text video.title ]
                , Html.div [ class "w-full" ]
                    [ Slider.slider
                        (Slider.config
                            |> Slider.setMin 0
                            |> Slider.setMax video.duration
                            |> Slider.setOnInput SetVideoTime
                            |> Slider.setValue (toFloat (round model.videoTime))
                            |> Slider.setStep 1
                            |> Slider.setAttributes [ style "margin" "0" ]
                        )
                    ]
                , Html.div []
                    [ Html.text
                        (formatTime model.videoTime
                            ++ " / "
                            ++ formatTime video.duration
                        )
                    ]
                , Html.div [ class "flex gap-2" ]
                    [ Fab.fab
                        (Fab.config
                            |> Fab.setOnClick FastRewind
                            |> Fab.setAttributes [ Theme.primaryBg ]
                        )
                        (Fab.icon "fast_rewind")
                    , if model.videoIsPlaying then
                        Fab.fab
                            (Fab.config
                                |> Fab.setOnClick PauseVideo
                                |> Fab.setAttributes [ Theme.primaryBg ]
                            )
                            (Fab.icon "pause")

                      else
                        Fab.fab
                            (Fab.config
                                |> Fab.setOnClick PlayVideo
                                |> Fab.setAttributes [ Theme.primaryBg ]
                            )
                            (Fab.icon "play_arrow")
                    , Fab.fab
                        (Fab.config
                            |> Fab.setOnClick FastForward
                            |> Fab.setAttributes [ Theme.primaryBg ]
                        )
                        (Fab.icon "fast_forward")
                    ]
                , Html.div []
                    [ Button.raised
                        (Button.config
                            |> Button.setIcon (Just (Button.icon "save"))
                            |> Button.setOnClick SaveRecording
                        )
                        "Save recording"
                    ]
                , Html.div []
                    (video.subtitleIds
                        |> List.filterMap (\subtitleId -> Dict.get subtitleId model.subtitles)
                        |> List.map
                            (\subtitle ->
                                Html.div []
                                    [ Html.div [ class "text-center" ] [ Html.text subtitle.text ]
                                    ]
                            )
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
                                Button.raised
                                    (Button.config
                                        |> Button.setIcon (Just (Button.icon "play_arrow"))
                                        |> Button.setOnClick (PlayRecording recording)
                                    )
                                    "Play recording"

                              else
                                Button.raised
                                    (Button.config
                                        |> Button.setIcon (Just (Button.icon "sync"))
                                        |> Button.setOnClick (LoadVideo recording.videoId)
                                    )
                                    "Load video"
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
    , videos : Dict VideoId Video
    , videosOrdered : List VideoId
    , recordings : List Recording
    , subtitles : Dict SubtitleId Subtitle
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
    , subtitleIds : List SubtitleId
    }


initVideos : List Video
initVideos =
    [ { id = "UwRZ8TY2Z4k"
      , title = "It's too hot this summer! Talk about our environment 今年夏天也太热了吧聊聊环保"
      , duration = 884.301
      , subtitleIds = []
      }
    , { id = "jKPlNHHYKZo"
      , title = "Difference between giving Chinese and English names 中英文起名字的区别"
      , duration = 2138.561
      , subtitleIds = []
      }
    , { id = "8FKWqzd5jjs"
      , title = "Talk about insomnia 失眠和晚睡强迫症"
      , duration = 1391.061
      , subtitleIds = []
      }
    ]


getVideo : Maybe VideoId -> Dict VideoId Video -> Maybe Video
getVideo videoId videos =
    videoId |> Maybe.andThen (\id -> Dict.get id videos)


init : () -> ( Model, Cmd Msg )
init () =
    ( { tabId = 0
      , videoId = Nothing
      , videoIsPlaying = False
      , videoTime = 0
      , videos =
            initVideos
                |> List.map (\video -> ( video.id, video ))
                |> Dict.fromList
      , videosOrdered = initVideos |> List.map (\video -> video.id)
      , recordings = []
      , subtitles = Dict.empty
      }
    , Cmd.batch
        [ Random.generate GetSubtitles subtitles1
        , Random.generate GetSubtitles subtitles2
        , Random.generate GetSubtitles subtitles3
        ]
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
    | GetSubtitles (List Subtitle)


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

        GetSubtitles subtitles ->
            let
                subtitlesDict =
                    subtitles
                        |> List.map (\subtitle -> ( subtitle.id, subtitle ))
                        |> Dict.fromList

                maybeVideoId =
                    subtitles |> List.head |> Maybe.map .videoId
            in
            case maybeVideoId of
                Nothing ->
                    ( model, Cmd.none )

                Just videoId ->
                    ( { model
                        | videos =
                            Dict.map
                                (\_ video ->
                                    if video.id == videoId then
                                        { video | subtitleIds = List.map .id subtitles }

                                    else
                                        video
                                )
                                model.videos
                        , subtitles = Dict.union subtitlesDict model.subtitles
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
    Html.div [ class "h-16 flex text-xl bg-white" ]
        (List.indexedMap (viewTab model) tabs)


viewTab : Model -> TabId -> TabData -> Html Msg
viewTab model tabId tab =
    Html.button
        [ classList [ ( "text-indigo-500 border-b-2 border-indigo-500", model.tabId == tabId ) ]
        , class "grow h-full flex justify-center items-center cursor-pointer"
        , onClick (TabClicked tabId)
        ]
        [ Html.text tab.text ]


viewVideoCard : Model -> Video -> Html Msg
viewVideoCard model video =
    Card.card (Card.config |> Card.setAttributes [ class "px-4 pt-4 mb-4" ])
        { blocks =
            ( Card.block <|
                Html.div []
                    [ Html.h2 [ class "text-xl" ] [ Html.text video.title ]
                    ]
            , []
            )
        , actions =
            Just <|
                Card.actions
                    { buttons =
                        [ Card.button
                            (Button.config
                                |> Button.setOnClick (ListenToVideo video.id)
                            )
                            "Listen"
                        ]
                    , icons =
                        if model.videoId == Just video.id then
                            if model.videoIsPlaying then
                                [ Card.icon
                                    (IconButton.config
                                        |> IconButton.setOnClick PauseVideo
                                    )
                                    (IconButton.icon "pause")
                                ]

                            else
                                [ Card.icon
                                    (IconButton.config
                                        |> IconButton.setOnClick PlayVideo
                                    )
                                    (IconButton.icon "play_arrow")
                                ]

                        else
                            []
                    }
        }


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
