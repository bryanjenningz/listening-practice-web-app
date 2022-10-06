port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra as List
import Material.Button as Button
import Material.Card as Card
import Material.Fab as Fab
import Material.Tab as Tab exposing (Tab)
import Material.TabBar as TabBar
import Material.Theme as Theme
import Time


type alias TabData =
    { text : String
    , icon : String
    , content : Model -> Html Msg
    }


findTabIndex : Int
findTabIndex =
    0


listenTabIndex : Int
listenTabIndex =
    1


firstTab : TabData
firstTab =
    { text = "Find"
    , icon = "search"
    , content = \_ -> Html.div [] (List.map viewVideoCard videos)
    }


remainingTabs : List TabData
remainingTabs =
    [ { text = "Listen"
      , icon = "headphones"
      , content =
            \model ->
                case model.selectedVideo of
                    Nothing ->
                        Html.div [ class "flex flex-col items-center" ]
                            [ Html.div [ class "mb-2 text-xl" ] [ Html.text "No video selected" ]
                            , Button.raised
                                (Button.config
                                    |> Button.setIcon (Just (Button.icon "search"))
                                    |> Button.setOnClick (TabClicked findTabIndex)
                                )
                                "Find a video"
                            ]

                    Just video ->
                        Html.div [ class "flex flex-col items-center gap-2" ]
                            [ Html.div [ class "text-xl text-center" ]
                                [ Html.text video.title ]
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
                                , if model.isPlaying then
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
                            ]
      }
    , { text = "Review"
      , icon = "grading"
      , content = \_ -> Html.text "Review"
      }
    , { text = "Profile"
      , icon = "person"
      , content = \_ -> Html.text "Profile"
      }
    ]


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


type alias VideoData =
    { id : String
    , title : String
    , duration : Float
    }


videos : List VideoData
videos =
    [ { id = "UwRZ8TY2Z4k"
      , title = "It's too hot this summer! Talk about our environment 今年夏天也太热了吧聊聊环保"
      , duration = 884.301
      }
    , { id = "jKPlNHHYKZo"
      , title = "Difference between giving Chinese and English names 中英文起名字的区别"
      , duration = 2138.561
      }
    , { id = "8FKWqzd5jjs"
      , title = "Talk about insomnia 失眠和晚睡强迫症"
      , duration = 1391.061
      }
    ]


type alias Model =
    { selectedTab : Int
    , selectedVideo : Maybe VideoData
    , isPlaying : Bool
    , videoTime : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { selectedTab = 0
      , selectedVideo = Nothing
      , isPlaying = False
      , videoTime = 0
      }
    , Cmd.none
    )


type Msg
    = TabClicked Int
    | ListenToVideo VideoData
    | PlayVideo
    | PauseVideo
    | FastForward
    | FastRewind
    | SetCurrentVideoTime Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked selectedTab ->
            ( { model | selectedTab = selectedTab }, Cmd.none )

        ListenToVideo video ->
            ( { model
                | selectedTab = listenTabIndex
                , selectedVideo = Just video
                , isPlaying = True
              }
            , startVideo video.id
            )

        PlayVideo ->
            ( { model | isPlaying = True }, playVideo () )

        PauseVideo ->
            ( { model | isPlaying = False }, pauseVideo () )

        FastForward ->
            ( model, fastForward () )

        FastRewind ->
            ( model, fastRewind () )

        SetCurrentVideoTime videoTime ->
            ( { model | videoTime = videoTime }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewTabs model ]
        , Html.div [ class "pt-24 px-3" ]
            [ (firstTab :: remainingTabs)
                |> List.getAt model.selectedTab
                |> Maybe.withDefault firstTab
                |> (\tab -> tab.content model)
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    TabBar.tabBar (TabBar.config |> TabBar.setStacked True)
        (viewTab model 0 firstTab)
        (List.indexedMap (\i tab -> viewTab model (i + 1) tab) remainingTabs)


viewTab : Model -> Int -> TabData -> Tab Msg
viewTab model index tab =
    Tab.tab
        (Tab.config
            |> Tab.setActive (model.selectedTab == index)
            |> Tab.setOnClick (TabClicked index)
        )
        { label = tab.text, icon = Just (Tab.icon tab.icon) }


viewVideoCard : VideoData -> Html Msg
viewVideoCard video =
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
                                |> Button.setOnClick (ListenToVideo video)
                            )
                            "Listen"
                        ]
                    , icons = []
                    }
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    currentVideoTime SetCurrentVideoTime


port startVideo : String -> Cmd msg


port playVideo : () -> Cmd msg


port pauseVideo : () -> Cmd msg


port fastForward : () -> Cmd msg


port fastRewind : () -> Cmd msg


port currentVideoTime : (Float -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
