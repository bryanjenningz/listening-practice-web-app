module Video exposing (Subtitle, Video, VideoId, VideoTime, decodeSubtitles)

import Dict
import Json.Decode as Json
import Parser exposing ((|.), (|=), Parser)


type alias VideoId =
    String


type alias VideoTime =
    Float


type alias Video =
    { id : VideoId
    , title : String
    , duration : Float
    , subtitles : List Subtitle
    }


type alias Subtitle =
    { videoId : VideoId
    , text : String
    , time : VideoTime
    }


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
        |. Parser.oneOf
            [ Parser.token "0"
            , Parser.succeed ()
            ]
        |= Parser.int
