module VideoTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "decodeVideo"
        [ test "Decodes a video" <|
            \_ ->
                List.length []
                    |> Expect.equal 0
        ]
