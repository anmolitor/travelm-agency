module File exposing (..)


type alias OutputFile =
    { name : String
    , language : Maybe String
    , extension : String
    , content : String
    }


type alias InputFile =
    { name : String
    , language : String
    , extension : String
    , content : String
    }


outputFileToPath : OutputFile -> String
outputFileToPath file =
    List.filterMap identity
        [ Just file.name
        , file.language
        , Just file.extension
        ]
        |> String.join "."


inputFileToPath : InputFile -> String
inputFileToPath file =
    [ file.name
    , file.language
    , file.extension
    ]
        |> String.join "."
