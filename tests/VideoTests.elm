module VideoTests exposing (suite)

import Expect
import Json.Decode as Json
import Test exposing (Test, describe, test)
import Video exposing (decodeVideo)


suite : Test
suite =
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
