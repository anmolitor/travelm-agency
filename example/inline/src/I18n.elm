module I18n exposing (I18n, de, en, greeting, languageSwitchInfo, order, order1, order10, order11, order12, order13, order14, order15, order16, order17, order18, order19, order2, order20, order3, order4, order5, order6, order7, order8, order9)

{-| This file was generated by elm-i18n.


-}


type alias I18n =
    { greeting_ : String -> String
    , languageSwitchInfo_ : String -> String
    , order_ : String -> String -> String
    , order1_ : String -> String -> String
    , order10_ : String -> String -> String
    , order11_ : String -> String -> String
    , order12_ : String -> String -> String
    , order13_ : String -> String -> String
    , order14_ : String -> String -> String
    , order15_ : String -> String -> String
    , order16_ : String -> String -> String
    , order17_ : String -> String -> String
    , order18_ : String -> String -> String
    , order19_ : String -> String -> String
    , order2_ : String -> String -> String
    , order20_ : String -> String -> String
    , order3_ : String -> String -> String
    , order4_ : String -> String -> String
    , order5_ : String -> String -> String
    , order6_ : String -> String -> String
    , order7_ : String -> String -> String
    , order8_ : String -> String -> String
    , order9_ : String -> String -> String
    }


greeting : I18n -> String -> String
greeting i18n_ name_ =
    i18n_.greeting_ name_


languageSwitchInfo : I18n -> String -> String
languageSwitchInfo i18n_ currentLanguage_ =
    i18n_.languageSwitchInfo_ currentLanguage_


order : I18n -> { a | language : String, name : String } -> String
order i18n_ placeholders_ =
    i18n_.order_ placeholders_.language placeholders_.name


order1 : I18n -> { a | language : String, name : String } -> String
order1 i18n_ placeholders_ =
    i18n_.order1_ placeholders_.language placeholders_.name


order10 : I18n -> { a | language : String, name : String } -> String
order10 i18n_ placeholders_ =
    i18n_.order10_ placeholders_.language placeholders_.name


order11 : I18n -> { a | language : String, name : String } -> String
order11 i18n_ placeholders_ =
    i18n_.order11_ placeholders_.language placeholders_.name


order12 : I18n -> { a | language : String, name : String } -> String
order12 i18n_ placeholders_ =
    i18n_.order12_ placeholders_.language placeholders_.name


order13 : I18n -> { a | language : String, name : String } -> String
order13 i18n_ placeholders_ =
    i18n_.order13_ placeholders_.language placeholders_.name


order14 : I18n -> { a | language : String, name : String } -> String
order14 i18n_ placeholders_ =
    i18n_.order14_ placeholders_.language placeholders_.name


order15 : I18n -> { a | language : String, name : String } -> String
order15 i18n_ placeholders_ =
    i18n_.order15_ placeholders_.language placeholders_.name


order16 : I18n -> { a | language : String, name : String } -> String
order16 i18n_ placeholders_ =
    i18n_.order16_ placeholders_.language placeholders_.name


order17 : I18n -> { a | language : String, name : String } -> String
order17 i18n_ placeholders_ =
    i18n_.order17_ placeholders_.language placeholders_.name


order18 : I18n -> { a | language : String, name : String } -> String
order18 i18n_ placeholders_ =
    i18n_.order18_ placeholders_.language placeholders_.name


order19 : I18n -> { a | language : String, name : String } -> String
order19 i18n_ placeholders_ =
    i18n_.order19_ placeholders_.language placeholders_.name


order2 : I18n -> { a | language : String, name : String } -> String
order2 i18n_ placeholders_ =
    i18n_.order2_ placeholders_.language placeholders_.name


order20 : I18n -> { a | language : String, name : String } -> String
order20 i18n_ placeholders_ =
    i18n_.order20_ placeholders_.language placeholders_.name


order3 : I18n -> { a | language : String, name : String } -> String
order3 i18n_ placeholders_ =
    i18n_.order3_ placeholders_.language placeholders_.name


order4 : I18n -> { a | language : String, name : String } -> String
order4 i18n_ placeholders_ =
    i18n_.order4_ placeholders_.language placeholders_.name


order5 : I18n -> { a | language : String, name : String } -> String
order5 i18n_ placeholders_ =
    i18n_.order5_ placeholders_.language placeholders_.name


order6 : I18n -> { a | language : String, name : String } -> String
order6 i18n_ placeholders_ =
    i18n_.order6_ placeholders_.language placeholders_.name


order7 : I18n -> { a | language : String, name : String } -> String
order7 i18n_ placeholders_ =
    i18n_.order7_ placeholders_.language placeholders_.name


order8 : I18n -> { a | language : String, name : String } -> String
order8 i18n_ placeholders_ =
    i18n_.order8_ placeholders_.language placeholders_.name


order9 : I18n -> { a | language : String, name : String } -> String
order9 i18n_ placeholders_ =
    i18n_.order9_ placeholders_.language placeholders_.name


de : I18n
de =
    { greeting_ = \name_ -> "Hallo " ++ name_
    , languageSwitchInfo_ =
        \currentLanguage_ -> "Du kannst hier deine Sprache von " ++ currentLanguage_ ++ " zu einer anderen ändern."
    , order_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order1_ =
        \language_ name_ ->
            "Bla ein langer Text loremipsum agawgavawawgawgwagawvawaawgawg Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order10_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order11_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order12_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order13_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order14_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order15_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order16_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order17_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order18_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order19_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order2_ =
        \language_ name_ ->
            "lawv f<uwaf<awf<wafawfawf<awf awfawfwafbf<f<lg<bwgl<wbgl<kbg<lwkgb<lkwgb<lkwgb<w Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order20_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order3_ =
        \language_ name_ ->
            "yaggvawfaw fpawfapwfpawlfp<lpfwlapflpwlfp<lwpflypflyplpylfpylfpflpyelfpyelfpylepflyepflypl Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order4_ =
        \language_ name_ ->
            "yslfvnwnwf <wof<wof<wof<wfo<kwfo<kwfo<kwof<kwofk<owkfo<kfwo<kwfo<kfo<kwf Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order5_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order6_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order7_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order8_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    , order9_ =
        \language_ name_ ->
            "Die Reihenfolge der benannten Platzhalter bleibt konsistent auch wenn die Sprachen sich ändern! Name: "
                ++ name_
                ++ ", Sprache: "
                ++ language_
    }


en : I18n
en =
    { greeting_ = \name_ -> "Hello " ++ name_
    , languageSwitchInfo_ =
        \currentLanguage_ -> "You may switch languages from " ++ currentLanguage_ ++ " to another one here."
    , order_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order1_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order2_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order3_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order4_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order5_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order6_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order7_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order8_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order9_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order10_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order11_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order12_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order13_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order14_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order15_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order16_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order17_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order18_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order19_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    , order20_ =
        \language_ name_ ->
            "The order of the named placeholder keys stays consistent even when switching languages! Language: "
                ++ language_
                ++ ", Name: "
                ++ name_
                ++ "."
    }
