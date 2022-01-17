module CodeGen.Shared exposing (Context, languageRelatedDecls, unwrapDecl, intlAnn, endoAnn, appendAll)

import Elm.CodeGen as CG
import Elm.Syntax.Declaration exposing (Declaration)
import Generators.Names exposing (Names)
import Intl exposing (Intl)
import String.Extra


type alias Context =
    { version : String
    , moduleName : CG.ModuleName
    , names : Names
    , intl : Intl
    }


unwrapDecl : CG.Declaration -> Declaration
unwrapDecl decl =
    case decl of
        CG.DeclWithComment _ f ->
            f ""

        CG.DeclNoComment d ->
            d

endoAnn : CG.TypeAnnotation -> CG.TypeAnnotation
endoAnn ann = CG.funAnn ann ann

intlAnn : CG.TypeAnnotation
intlAnn =
    CG.fqTyped [ "Intl" ] "Intl" []

appendAll : CG.Expression -> List CG.Expression -> CG.Expression
appendAll =
    List.foldl (\before new -> CG.applyBinOp new CG.append before)    


languageRelatedDecls : Names -> List String -> ( List CG.Declaration, List CG.TopLevelExpose )
languageRelatedDecls names languages =
    ( [ languageTypeDecl names languages
      , languagesDecl names languages
      , languageToStringDecl names languages
      , languageFromStringDecl names languages
      ]
    , [ CG.openTypeExpose names.languageTypeName
      , CG.funExpose names.languagesName
      , CG.funExpose names.languageToStringFunName
      , CG.funExpose names.languageFromStringFunName
      ]
    )


languageTypeDecl : Names -> List String -> CG.Declaration
languageTypeDecl names languages =
    CG.customTypeDecl (Just (CG.emptyDocComment |> CG.markdown "Enumeration of the supported languages")) names.languageTypeName [] <|
        List.map (String.Extra.classify >> (\lang -> ( lang, [] ))) languages


languagesDecl : Names -> List String -> CG.Declaration
languagesDecl names languages =
    CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "A list containing all `Language`s"))
        (Just <| CG.listAnn <| CG.typed names.languageTypeName [])
        names.languagesName
        (CG.list <| List.map (String.Extra.classify >> CG.val) languages)


languageToStringDecl : Names -> List String -> CG.Declaration
languageToStringDecl names languages =
    CG.funDecl (Just (CG.emptyDocComment |> CG.markdown "Convert a `Language` to its `String` representation."))
        (Just <| CG.funAnn (CG.typed names.languageTypeName []) CG.stringAnn)
        names.languageToStringFunName
        [ CG.varPattern "lang_" ]
        (CG.caseExpr (CG.val "lang_") <|
            List.map (\lang -> ( CG.namedPattern (String.Extra.classify lang) [], CG.string lang )) languages
        )


languageFromStringDecl : Names -> List String -> CG.Declaration
languageFromStringDecl names languages =
    CG.funDecl (Just (CG.emptyDocComment |> CG.markdown """Maybe parse a `Language` from a `String`. 
This only considers the keys given during compile time, if you need something like 'en-US' to map to the correct `Language`,
you should write your own parsing function."""))
        (Just <| CG.funAnn CG.stringAnn (CG.maybeAnn <| CG.typed names.languageTypeName []))
        names.languageFromStringFunName
        [ CG.varPattern "lang_" ]
        (CG.caseExpr (CG.val "lang_") <|
            List.map (\lang -> ( CG.stringPattern lang, CG.apply [ CG.fun "Just", CG.val <| String.Extra.classify lang ] )) languages
                ++ [ ( CG.allPattern, CG.val "Nothing" ) ]
        )
