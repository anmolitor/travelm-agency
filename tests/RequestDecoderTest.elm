module RequestDecoderTest exposing (..)

import Expect
import Json.Decode as D
import Json.Encode as E
import Ports exposing (Request(..), requestDecoder)
import Test exposing (Test, describe, test)
import Types exposing (TSegment(..))
import Ports exposing (GeneratorMode(..))


suite : Test
suite =
    describe "Request Decoder"
        [ test "decode translation request" <|
            \_ ->
                D.decodeString requestDecoder """{
  "type": "translation",
  "fileName": "demo.en.json",
  "fileContent": "{\\"demoKey\\": \\"demoValue\\"}"
}"""
                    |> Expect.equal
                        (Ok <|
                            AddTranslation
                                { content = [ ( "demoKey", ( Text "demoValue", [] ) ) ]
                                , identifier = "demo"
                                , language = "en"
                                }
                        )
        , test "decode finish request" <|
            \_ ->
                D.decodeString requestDecoder """{
  "type": "finish",
  "elmModuleName": "Test.elm",
  "generatorMode": "inline",
  "addContentHash": true
}"""
                    |> Expect.equal
                        (Ok <|
                            FinishModule
                                { elmModuleName = "Test.elm"
                                , generatorMode = Inline
                                , addContentHash = True
                                }
                        )
        ]
