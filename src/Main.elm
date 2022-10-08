port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import List.Extra as List
import Material.Button as Button
import Material.Card as Card
import Material.Fab as Fab
import Material.IconButton as IconButton
import Material.Slider as Slider
import Material.Tab as Tab exposing (Tab)
import Material.TabBar as TabBar
import Material.Theme as Theme
import Random exposing (Generator)


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


firstTab : TabData
firstTab =
    { text = "Find"
    , icon = "search"
    , content = viewFindTab
    }


remainingTabs : List TabData
remainingTabs =
    [ { text = "Listen"
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
                , case
                    List.getAt model.subtitleIndex video.subtitleIds
                        |> Maybe.andThen (\subtitleId -> Dict.get subtitleId model.subtitles)
                  of
                    Nothing ->
                        Html.text ""

                    Just subtitle ->
                        Html.div []
                            [ Html.div [ class "text-center" ] [ Html.text subtitle.text ]
                            , Html.div [ class "text-center" ] [ Html.text (formatTime subtitle.time) ]
                            , Html.div [ class "flex gap-2" ]
                                [ Fab.fab
                                    (Fab.config
                                        |> Fab.setOnClick PrevSubtitle
                                        |> Fab.setAttributes [ Theme.primaryBg ]
                                    )
                                    (Fab.icon "keyboard_double_arrow_left")
                                , Fab.fab
                                    (Fab.config
                                        |> Fab.setOnClick SetSubtitleTime
                                        |> Fab.setAttributes [ Theme.primaryBg ]
                                    )
                                    (Fab.icon "save")
                                , Fab.fab
                                    (Fab.config
                                        |> Fab.setOnClick NextSubtitle
                                        |> Fab.setAttributes [ Theme.primaryBg ]
                                    )
                                    (Fab.icon "keyboard_double_arrow_right")
                                ]
                            ]
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
    , subtitleIndex : Int
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


type alias SubtitleId =
    Int


type alias Subtitle =
    { id : SubtitleId
    , videoId : VideoId
    , text : String
    , time : VideoTime
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
      , subtitleIndex = 0
      , subtitles = Dict.empty
      }
    , Random.generate GetSubtitles subtitles1
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
    | SetSubtitleTime
    | NextSubtitle
    | PrevSubtitle


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

        SetSubtitleTime ->
            let
                maybeSubtitle : Maybe Subtitle
                maybeSubtitle =
                    getVideo model.videoId model.videos
                        |> Maybe.andThen (\video -> List.getAt model.subtitleIndex video.subtitleIds)
                        |> Maybe.andThen (\subtitleId -> Dict.get subtitleId model.subtitles)
            in
            case maybeSubtitle of
                Nothing ->
                    ( model, Cmd.none )

                Just subtitle ->
                    ( { model
                        | subtitles =
                            Dict.update subtitle.id
                                (Maybe.map (\sub -> { sub | time = model.videoTime }))
                                model.subtitles
                      }
                    , Cmd.none
                    )

        NextSubtitle ->
            let
                subtitlesLength =
                    getVideo model.videoId model.videos
                        |> Maybe.map (\video -> List.length video.subtitleIds)
                        |> Maybe.withDefault 0
            in
            ( { model | subtitleIndex = min (subtitlesLength - 1) (model.subtitleIndex + 1) }
            , Cmd.none
            )

        PrevSubtitle ->
            ( { model | subtitleIndex = max 0 (model.subtitleIndex - 1) }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "fixed w-full" ] [ viewTabs model ]
        , Html.div [ class "pt-24 px-3" ]
            [ (firstTab :: remainingTabs)
                |> List.getAt model.tabId
                |> Maybe.withDefault firstTab
                |> (\tab -> tab.content model)
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    TabBar.tabBar (TabBar.config |> TabBar.setStacked True)
        (viewTab model 0 firstTab)
        (List.indexedMap (\i tab -> viewTab model (i + 1) tab) remainingTabs)


viewTab : Model -> TabId -> TabData -> Tab Msg
viewTab model tabId tab =
    Tab.tab
        (Tab.config
            |> Tab.setActive (model.tabId == tabId)
            |> Tab.setOnClick (TabClicked tabId)
        )
        { label = tab.text, icon = Just (Tab.icon tab.icon) }


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


generateSubtitleId : Generator Int
generateSubtitleId =
    Random.int Random.minInt Random.maxInt


generateSubtitles : VideoId -> String -> Generator (List Subtitle)
generateSubtitles videoId subtitles =
    subtitles
        |> String.split "\n"
        |> (\lines ->
                Random.list (List.length lines) generateSubtitleId
                    |> Random.map
                        (\subtitleIds ->
                            List.map2
                                (\subtitleId line ->
                                    { id = subtitleId
                                    , videoId = videoId
                                    , text = line
                                    , time = 0
                                    }
                                )
                                subtitleIds
                                lines
                        )
           )


subtitles1 : Generator (List Subtitle)
subtitles1 =
    generateSubtitles "UwRZ8TY2Z4k" """哈喽大家好
欢迎来到聊聊东西
我们聊东西文化
也聊很多东西
我是 Candice
今天又是我一个人
因为现在是我们的国庆假期
一个星期的假期
所以大家都休息啊
出去玩什么的
我也不想去找 yifei
或者是别的朋友来跟我一起
录音我想他们可以多休息一下
那今天就是我一个人跟大家聊一聊
其实我也有好几个话题备选
但是刚刚打开手机
打开微博
看到一个热搜话题
就是七个省将出现三十七度以上的高温
包括我在的城市也是特别热
这几天三十七度
明天三十八度
真的太热了
现在已经是十月份了
就是让人难以置信的为什么
连续的高温
而且整个夏天也是非常的热
包括我有听说欧洲
虽然我没有去过欧洲
欧洲整个夏天非常热
然后欧洲人办公室有空调
家里没有空调
那晚上睡觉也是非常热
我听 yifei 说
因为 yifei 以前不是在英国留学吗
夏天的时候就是很凉快吗
夏天可能二十多度
哇我觉得这真是太舒服的夏天了
这个温度就是刚刚好啊
你也不会出汗
然后非常舒服的夏天
但是现在也不复存在了
我还看到以前
两个月可能看到说美国加州也是
缺水干旱
是特别严重的那种缺水干旱
包括中国在内农作物减产
然后因为太热没有没有水吗
缺水那些蔬菜啊
水果啊一些树木啊
他们都长不好
那当然物价就会上涨
物价上涨当然有很多的原因了
包括疫情的原因
有很多别的原因
但是我觉得今年夏天
这个天气也是原因之一啊
所以为什么我们的夏天越来越热
持续高温
没有雨持续干旱
真的让人觉得怎么回事啊
这个地球怎么了
因为这样
我就想自己去找一些纪录片看一下
今年初我开始看一些纪录片
印象比较深刻的两个纪录片
一个叫 chasing ice
中文翻译叫逐冰之旅
那一个摄影团队在
南极拍摄这个冰的变化
天气的变化
那他们拍摄的结果就是发现
变化真的是出乎意料的惊人
最后的数据统计就是过去十年
这个整个冰融化的总量
比过去一百年都多
所以可能很多的南极的一些动物
一些生物他们都失去了原来的家
北极熊啊
企鹅啊因为这个气候变暖
他们的食物减少了
他们捕捞食物的过程变得更加艰难
他们更难生存了
然后冰融化之后
当然整个海平面要上升
然后冰融化之后
可能也会导致一些病毒
古老的病毒再次出现在世界上
可能温度高
也会更适宜一些病毒的繁衍
生殖
那我看完这个逐冰之旅之后
又看了一个叫 chasing coral
他们好像是一个系列的纪录片
这个中文名叫追逐珊瑚
看完之后也让我觉得挺绝望的
比这个 chasing ice 更让人觉得绝望
因为这个摄影团队
他们知道珊瑚在每一年减少
但是他们去拍摄的过程发现
珊瑚减少的速度
死亡的速度
比他们想象的快的多的时候
真的让人感觉到非常绝望
因为珊瑚在整个海洋系统里面
是一个非常基本保持海洋
生态的一个生物
那如果海洋里没有了珊瑚
那些小动物他就没有了家
没有了食物的来源
那他们就会减少数量
相应的大的动物没有了
这些小动物去当食物的话
他们也会逐渐的灭绝
所以生物多样性受到影响
物种也会因为这件事情有灭绝的可能
所以我不知道每次看完纪录片
就会觉得人类很渺小
人类出现的这短短的这段时间里
对整个世界
对整个地球产生的影响太大了
改变太多了
我很难说这样的改变
是好的还是不好的
你看这个追逐珊瑚也是会发现
海洋里面的垃圾太多了
塑料瓶塑料袋
各种塑料出现在纪录片的镜头里
他们没有办法短期内得到分解
一直在海洋里堆积产生这些垃圾
而人们又
没有一些好的办法去解决这个问题
挺难的
当年发明塑料的人
大家都觉得是一个很聪明很有用
很轻很耐用很便宜的一种材料
但是没有想到这么多年过去之后
他会成为一个问题
但是现在可能
人们还没有一个很
好的方式去解决这个问题
再说垃圾分类这个问题
我看的另一个纪录片
我不太记得他的名字叫什么
他就是在讲这些垃圾是怎么处理的
很多国家比如说包括日本啊
比如说一个塑料瓶
他的瓶盖和他的瓶身
好像都应该是分
在不同的垃圾种类里面
分的很细
我看的这个纪录片是讲的美国的情况
我不知道日本或者是其他的国家
是什么样的情况
这个专门做垃圾回收的
做的很大的一个公司说
这些垃圾分类了之后也没有办法
这些塑料瓶
塑料袋
就算是已经分类到塑料这一类了
但是他们能做的只是把这些垃圾
放到一起
可以把它填埋在地上
就是埋在地下
让他几百年几千年慢慢的分解
或者是运到一些发展中国家填埋
以前中国其实是很
很多一些发达国家呃的垃圾的目的地
这很多的呃
外国的垃圾会运到中国来填埋
但是现在可能因为中国的垃圾太多了
中国政府
有一些措施会阻止这样的情况发生
所以到最后
就算你很
用心的做好了垃圾分类这件事情
整个地球还是被这些垃圾包围了
人们没有想出更好的办法
所以我们能做什么呢
这个是我们应该想的问题
可能听到这有的人会想哈
你操这么多心做什么呀
我们都是普通的人
我们能做什么呢
现在全球变暖了我可以做什
我们什么也做不了
我觉得大部分人都是这样想的
包括我自己以前也是这样想的
我不是以前还讨论过吃素这个话题吗
那有一些我认识的人
他们吃素是因为他们的宗教信仰
或者是因为他们觉得动物很可爱
不应该杀死动物们
但还有一个很重要的原因
就是他们觉得他们是在保护环境
因为饲养的家畜
其实排放了很多的二氧化碳
一氧化二氮这样的东西
排放了很多的废气会导致全球变暖
其实他们就是在用自己的力量
为啊这个全球变暖做一点点贡献吧
保护环境
减少全球变暖
做一点点他们可以做的事情
但是并没有说要求每个人去吃素
因为我自己也做不到
但是如果你知道了这些事情之后
可能你会
想办法去做一点点什么来保护地球
保护环境
其实有时候如果心情
不是那么好的时候
我会看一些这样的纪录片
看一些比如说地球的繁衍过程
然后一些动物世界
植物世界这样的一些纪录片
就会让我烦恼减少一点点
因为我觉得这个世界太大
这个宇宙太大了
人的每一天的烦恼那么那么的渺小
你在烦恼你每天吃的东西
你的工作
你的学习
放在整个地球看
放在整个宇宙看
这都是特别特别小的事情
人真的很渺小
能做的事情很少
但是如果每个人都这样想的话
都什么都不去做的话
整个环境会越来越差
可能
我觉得对我们这一代的影响还不太大
到我们的孙子孙女那一代人的时候
会不会
他们已经都没有干净的空气可以使用
没有干净的空气
干净的水
不可以自由自在的在那个草地上奔跑
我不知道会发生什么
如果我们从现在开始
做一点点我们力所能及的事情
让身边的人知道这些事情
会不会间接的影响一些有能力的人呢
比如说我可能没有能力
但是我在我的播客里说了
那在我的听众里有一些很聪明
很厉害的人
他发明了一个
可以分解塑料的一种东西
那是不是就是一个很大很大的成就
很大很大的帮助呢
我看了这些纪录片
或者是别人听了我的播客
他决定
可能从明天开始他不用塑料袋
他每次去超市买东西
他都用那个环保袋
用布的袋子
或者是什么别的可以更容易降解
或者循环使用的材料
我在韩国的时候
那都很多年以前了
韩国人就已经开始
带自己的杯子去买咖啡
带自己的杯子买咖啡可以少
那个时候可以少五百韩币
就是带自己的杯子去买咖啡
少不了多少钱
但是又很麻烦
因为你要带个很大的杯子去
然后背着他回家还得洗对不对
很麻烦但是那个时候已经有
很多韩国人这样做了
后来我的一个韩国学生还告诉我
他们还会用自己的吸管
可能是买一个金属的吸管
每一天洗
这样就不用那些塑料的吸管
我觉得这些都是很好的一些习惯
当然可能我自己平时不喝咖啡
所以我很少会去咖啡店买咖啡
但是我很喜欢喝那个三得利的乌龙茶
污染挺严重的
你想每一瓶
乌龙茶都用一个塑料瓶装着
那如果我非得喝的话
我可以比如说买个大瓶子的
那就是可能一个大瓶
差不多是三个小瓶
那我买个大瓶
可能好一点点吧
就是减少一点点塑料瓶
这个可能是我能做到的一些事情
或者是我减少使用空调
就早上不要用
中午最热的时候用一用
然后晚上睡觉的时候
太热的时候用一用
还有就是多用走路啊
或者是骑共享单车啊
用公共交通更多
其实这个是因为我不喜欢开车
但也要看国情对吗
如果你是在欧洲
或者是在美国
你们可能必须使用到车
因为有时候你坐地铁
或者是坐公交车不太方便吧
你必须开车
比如说你可以一个星期去一次
多买一点这样就是减少开车的次数
我觉得我还要再去想一想
我还可以
在我的能力范围内
做一点什么小小的事情
可能我是一个无知的人
我不是一个啊很有能力
可以用我的科学技术去改变
现在这个情况
我也做不到
那我只能看看我可以做点什么
比如说减少用塑料的东西
还有一些人
的观念是觉得全球变暖是一个骗局
是一些政客为了自己的目的
编造的谎言
我不知道全球变暖是不是真的
这些数据是不是真的我不知道
我只是根据我自己的经验来讲
可能我小的时候的夏天没有这么热
我小的时候的冬天下雪会更厚一些
下雪的时间更长一些
现在的冬天下雪很快就结束了
可能下的雪第二天第三天就化了
非常非常少
作为一个想让地球更好的人
或者是想让我们的后代
生活的更好的人
我们可以做一点点力所能及的事情
那么这么一点点力所能及的事情
可能都会对保护环境起到点点的帮助
那这些东西堆积成一座小山
可能会有一些影响
最后谢谢大家的收听
如果是我的中国听众
当然我祝你十一假期快乐
好好的休息
因为工作很累了
休息好了可以更努力的去工作赚钱
那如果是我外国的
听众的话希望你们有一个好的周末
那我们下次见啦拜拜"""
