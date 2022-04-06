module RequestDecoderTest exposing (..)

import Dict
import Expect
import Json.Decode as D
import Ports exposing (GeneratorMode(..), Request(..), requestDecoder)
import Test exposing (Test, describe, test)
import Types.Segment as Segment
import Util exposing (emptyIntl)


suite : Test
suite =
    describe "Request Decoder"
        [ test "decode translation request" <|
            \_ ->
                D.decodeString (requestDecoder emptyIntl) """{
  "type": "translation",
  "fileName": "demo.en.json",
  "fileContent": "{\\"demoKey\\": \\"demoValue\\"}"
}"""
                    |> Expect.equal
                        (Ok <|
                            AddTranslation
                                { content =
                                    { pairs = Dict.fromList [ ( "demoKey", ( Segment.Text "demoValue", [] ) ) ]
                                    , fallback = Nothing
                                    , resources = ()
                                    }
                                , identifier = "demo"
                                , language = "en"
                                }
                        )
        , test "decode finish request" <|
            \_ ->
                D.decodeString (requestDecoder emptyIntl) """{
  "type": "finish",
  "elmModuleName": "Test.elm",
  "generatorMode": "inline",
  "addContentHash": true,
  "i18nArgLast": true
}"""
                    |> Expect.equal
                        (Ok <|
                            FinishModule
                                { elmModuleName = "Test.elm"
                                , generatorMode = Inline
                                , addContentHash = True
                                , i18nArgLast = True
                                }
                        )
        ]
