module Video exposing (Subtitle, Video, VideoId, VideoTime, decodeVideo, getNextSubtitle, getPrevSubtitle, getSubtitleAt)

import Dict
import Json.Decode as Json
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)


type alias VideoId =
    String


type alias VideoTime =
    Float


type alias Video =
    { id : VideoId
    , title : String
    , duration : VideoTime
    , subtitles : List Subtitle
    }


type alias Subtitle =
    { videoId : VideoId
    , text : String
    , time : VideoTime
    }


getSubtitleAt : VideoTime -> List Subtitle -> Maybe Subtitle
getSubtitleAt videoTime subtitles =
    case List.filter (\sub -> videoTime >= sub.time) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle


getNextSubtitle : VideoTime -> List Subtitle -> Maybe Subtitle
getNextSubtitle videoTime subtitles =
    List.find (\sub -> videoTime < sub.time) subtitles


getPrevSubtitle : VideoTime -> List Subtitle -> Maybe Subtitle
getPrevSubtitle videoTime subtitles =
    let
        timeTolerance =
            0.8
    in
    case List.filter (\sub -> videoTime >= sub.time + timeTolerance) subtitles |> List.last of
        Nothing ->
            List.head subtitles

        subtitle ->
            subtitle


decodeVideo : String -> Json.Decoder Video
decodeVideo videoId =
    Json.map4 Video
        (Json.succeed videoId)
        (Json.field "title" Json.string)
        (Json.field "duration"
            (Json.string
                |> Json.map
                    (fromStrTime >> Maybe.map toFloat >> Maybe.withDefault 3600)
            )
        )
        (Json.field "subtitles" (decodeSubtitles videoId))


decodeSubtitles : String -> Json.Decoder (List Subtitle)
decodeSubtitles videoId =
    Json.dict Json.string
        |> Json.map Dict.toList
        |> Json.map (List.map (Tuple.mapFirst (fromStrTime >> Maybe.withDefault -1)))
        |> Json.map (List.sortBy Tuple.first)
        |> Json.map (List.map (\( time, text ) -> Subtitle videoId text (toFloat time)))


fromStrTime : String -> Maybe Int
fromStrTime strTime =
    Parser.run parseTime strTime |> Result.toMaybe


parseTime : Parser Int
parseTime =
    Parser.succeed
        (\a b c ->
            case ( a, b, c ) of
                ( minutes, seconds, Nothing ) ->
                    minutes * 60 + seconds

                ( hours, minutes, Just seconds ) ->
                    hours * 3600 + minutes * 60 + seconds
        )
        |= parseInt
        |. Parser.symbol ":"
        |= parseInt
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ":"
                |= (parseInt |> Parser.map Just)
            , Parser.succeed Nothing
            ]


parseInt : Parser Int
parseInt =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.backtrackable <|
                Parser.succeed identity
                    |. Parser.token "0"
                    |= Parser.int
            , Parser.int
            ]
