module VideoTests exposing (..)

import Expect
import Json.Decode as Json
import Test exposing (Test, describe, test)
import Video exposing (decodeVideo, getSubtitleAt)


decodeVideoTests : Test
decodeVideoTests =
    describe "decodeVideo"
        [ test "Fails on empty object" <|
            \_ ->
                Json.decodeString (decodeVideo "a") """{}"""
                    |> Expect.err
        , test "Fails on object with not all properties 1" <|
            \_ ->
                Json.decodeString (decodeVideo "a") """{ "title": "b" }"""
                    |> Expect.err
        , test "Fails on object with not all properties 2" <|
            \_ ->
                Json.decodeString (decodeVideo "a")
                    """{ "title": "b", "duration": "1:00" }"""
                    |> Expect.err
        , test "Decodes an object 1" <|
            \_ ->
                Json.decodeString (decodeVideo "a")
                    """
                    {
                        "title": "b",
                        "duration": "1:00",
                        "subtitles": {}
                    }
                    """
                    |> Expect.equal (Ok { duration = 60, id = "a", subtitles = [], title = "b" })
        , test "Decodes an object 2" <|
            \_ ->
                Json.decodeString (decodeVideo "a")
                    """
                    {
                        "title": "b",
                        "duration": "1:10:05",
                        "subtitles": {
                            "0:03": "first",
                            "0:07": "second",
                            "1:02:03": "fourth",
                            "1:01": "third"
                        }
                    }
                    """
                    |> Expect.equal
                        (Ok
                            { duration = 4205
                            , id = "a"
                            , subtitles =
                                [ { text = "first", time = 3, videoId = "a" }
                                , { text = "second", time = 7, videoId = "a" }
                                , { text = "third", time = 61, videoId = "a" }
                                , { text = "fourth", time = 3723, videoId = "a" }
                                ]
                            , title = "b"
                            }
                        )
        , test "Decodes an object 3" <|
            \_ ->
                Json.decodeString (decodeVideo "a1")
                    """
                    {
                        "title": "a2",
                        "duration": "59:59",
                        "subtitles": {
                            "0:00": "first",
                            "0:01": "second",
                            "2:02": "third",
                            "10:03": "fourth"
                        }
                    }
                    """
                    |> Expect.equal
                        (Ok
                            { duration = 3599
                            , id = "a1"
                            , subtitles =
                                [ { text = "first", time = 0, videoId = "a1" }
                                , { text = "second", time = 1, videoId = "a1" }
                                , { text = "third", time = 122, videoId = "a1" }
                                , { text = "fourth", time = 603, videoId = "a1" }
                                ]
                            , title = "a2"
                            }
                        )
        ]


getSubtitleAtTests : Test
getSubtitleAtTests =
    describe "getSubtitleAt"
        [ test "Returns nothing if there are no subtitles" <|
            \_ ->
                getSubtitleAt 5 [] |> Expect.equal Nothing
        , test "Returns the first subtitle if the time is before all subtitles 1" <|
            \_ ->
                getSubtitleAt 5
                    [ { videoId = "a", text = "b", time = 123 } ]
                    |> Expect.equal (Just { videoId = "a", text = "b", time = 123 })
        , test "Returns the first subtitle if the time is before all subtitles 2" <|
            \_ ->
                getSubtitleAt 5
                    [ { videoId = "a", text = "b", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    ]
                    |> Expect.equal (Just { videoId = "a", text = "b", time = 123 })
        , test "Returns the first subtitle that the time is at or above 1" <|
            \_ ->
                getSubtitleAt 123
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a1", text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 2" <|
            \_ ->
                getSubtitleAt 124
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a1", text = "b1", time = 123 })
        , test "Returns the first subtitle that the time is at or above 3" <|
            \_ ->
                getSubtitleAt 456
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a2", text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 4" <|
            \_ ->
                getSubtitleAt 788
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a2", text = "b2", time = 456 })
        , test "Returns the first subtitle that the time is at or above 5" <|
            \_ ->
                getSubtitleAt 789
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a3", text = "b3", time = 789 })
        , test "Returns the first subtitle that the time is at or above 6" <|
            \_ ->
                getSubtitleAt 9999
                    [ { videoId = "a1", text = "b1", time = 123 }
                    , { videoId = "a2", text = "b2", time = 456 }
                    , { videoId = "a3", text = "b3", time = 789 }
                    ]
                    |> Expect.equal (Just { videoId = "a3", text = "b3", time = 789 })
        ]
