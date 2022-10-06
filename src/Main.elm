port module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import List.Extra as List
import Material.Button as Button
import Material.Card as Card
import Material.Tab as Tab exposing (Tab)
import Material.TabBar as TabBar
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

                    Just videoId ->
                        Html.div [ class "flex flex-col items-center" ]
                            [ Html.div [ class "mb-2 text-xl text-center" ]
                                [ List.find (.id >> (==) videoId) videos
                                    |> Maybe.map .title
                                    |> Maybe.withDefault ""
                                    |> Html.text
                                ]
                            , Html.div [] [ Html.text (formatTime model.videoTime) ]
                            , if model.isPlaying then
                                Button.raised
                                    (Button.config
                                        |> Button.setIcon (Just (Button.icon "pause"))
                                        |> Button.setOnClick PauseVideo
                                    )
                                    "Pause video"

                              else
                                Button.raised
                                    (Button.config
                                        |> Button.setIcon (Just (Button.icon "play_arrow"))
                                        |> Button.setOnClick PlayVideo
                                    )
                                    "Play video"
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
    }


videos : List VideoData
videos =
    [ { id = "UwRZ8TY2Z4k"
      , title = "It's too hot this summer! Talk about our environment 今年夏天也太热了吧聊聊环保"
      }
    , { id = "jKPlNHHYKZo"
      , title = "Difference between giving Chinese and English names 中英文起名字的区别"
      }
    , { id = "8FKWqzd5jjs"
      , title = "Talk about insomnia 失眠和晚睡强迫症"
      }
    ]


type alias Model =
    { selectedTab : Int
    , selectedVideo : Maybe String
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
    | ListenToVideo String
    | PlayVideo
    | PauseVideo
    | SetCurrentVideoTime Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabClicked selectedTab ->
            ( { model | selectedTab = selectedTab }, Cmd.none )

        ListenToVideo videoId ->
            ( { model
                | selectedTab = listenTabIndex
                , selectedVideo = Just videoId
                , isPlaying = True
              }
            , startVideo videoId
            )

        PlayVideo ->
            ( { model | isPlaying = True }, playVideo () )

        PauseVideo ->
            ( { model | isPlaying = False }, pauseVideo () )

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
                                |> Button.setOnClick (ListenToVideo video.id)
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


port currentVideoTime : (Float -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
