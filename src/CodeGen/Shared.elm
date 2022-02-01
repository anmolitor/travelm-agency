module CodeGen.Shared exposing (Context, addDeclaration, addDeclarations, addExposing, addExposings, addLanguageRelatedDeclsUnique, appendAll, emptyFile, endoAnn, finishFile, intlAnn, languageRelatedDecls)

import CodeGen.Utils
import Elm.CodeGen as CG
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node
import Elm.Syntax.Range as Range
import Generators.Names exposing (Names)
import Intl exposing (Intl)
import State exposing (NonEmptyState)
import String.Extra
import Types.UniqueName as Unique


type alias Context =
    { version : String
    , moduleName : CG.ModuleName
    , names : Names
    , intl : Intl
    }


endoAnn : CG.TypeAnnotation -> CG.TypeAnnotation
endoAnn ann =
    CG.funAnn ann ann


intlAnn : CG.TypeAnnotation
intlAnn =
    CG.fqTyped [ "Intl" ] "Intl" []


appendAll : CG.Expression -> List CG.Expression -> CG.Expression
appendAll =
    List.foldl (\before new -> CG.applyBinOp new CG.append before)


emptyFile : Context -> CG.File
emptyFile ctx =
    let
        fileComment =
            CG.emptyFileComment |> CG.markdown ("This file was generated by elm-i18n version " ++ ctx.version ++ ".")
    in
    CG.file (CG.normalModule ctx.moduleName [])
        []
        []
        (Just fileComment)


addDeclaration : CG.Declaration -> CG.File -> CG.File
addDeclaration decl =
    addDeclarations [ decl ]


addDeclarations : List CG.Declaration -> CG.File -> CG.File
addDeclarations decls file =
    { file | declarations = file.declarations ++ decls }


addExposing : CG.TopLevelExpose -> CG.File -> CG.File
addExposing newExpose =
    addExposings [ newExpose ]


addExposings : List CG.TopLevelExpose -> CG.File -> CG.File
addExposings newExposes file =
    let
        addExposingToModule : CG.Module -> CG.Module
        addExposingToModule mod =
            case mod of
                NormalModule m ->
                    NormalModule { m | exposingList = Node.map addExposingToExposing m.exposingList }

                PortModule m ->
                    PortModule { m | exposingList = Node.map addExposingToExposing m.exposingList }

                EffectModule m ->
                    EffectModule { m | exposingList = Node.map addExposingToExposing m.exposingList }

        addExposingToExposing : CG.Exposing -> CG.Exposing
        addExposingToExposing exp =
            case exp of
                Exposing.All _ ->
                    CG.exposeExplicit newExposes

                Exposing.Explicit previousExposes ->
                    CG.exposeExplicit <| List.map Node.value previousExposes ++ newExposes
    in
    { file | moduleDefinition = Node.map addExposingToModule file.moduleDefinition }


addLanguageRelatedDeclsUnique :
    Unique.UniqueNameContext { ctx | names : Names, state : NonEmptyState any, file : CG.File }
    -> Unique.UniqueNameContext { ctx | names : Names, state : NonEmptyState any, file : CG.File }
addLanguageRelatedDeclsUnique =
    Unique.map <|
        \ctx ->
            let
                ( decls, exposes ) =
                    languageRelatedDecls ctx.names (State.getLanguages ctx.state)
            in
            { ctx | file = ctx.file |> addDeclarations decls |> addExposings exposes }


finishFile : CG.File -> CG.File
finishFile file =
    { file
        | imports =
            CodeGen.Utils.extractImports file.declarations
                |> CodeGen.Utils.dictToImports
                |> List.map (Node.Node Range.emptyRange)
    }


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
    CG.valDecl (Just (CG.emptyDocComment |> CG.markdown "A list containing all `Language`s. The list is sorted alphabetically."))
        (Just <| CG.listAnn <| CG.typed names.languageTypeName [])
        names.languagesName
        (CG.list <| List.map (String.Extra.classify >> CG.val) <| List.sort languages)


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
This will map languages based on the prefix i.e. 'en-US' and 'en' will both map to 'En' unless you provided a 'en-US' translation file."""))
        (Just <| CG.funAnn CG.stringAnn (CG.maybeAnn <| CG.typed names.languageTypeName []))
        names.languageFromStringFunName
        [ CG.varPattern "lang" ]
    <|
        CG.letExpr
            [ CG.letFunction "helper" [ CG.varPattern "langs" ] <|
                CG.caseExpr
                    (CG.val "langs")
                    [ ( CG.listPattern [], CG.fqVal [ "Maybe" ] "Nothing" )
                    , ( CG.unConsPattern (CG.varPattern "l") (CG.varPattern "ls")
                      , CG.ifExpr
                            (CG.apply
                                [ CG.fqFun [ "String" ] "startsWith"
                                , CG.parens <| CG.apply [ CG.val names.languageToStringFunName, CG.val "l" ]
                                , CG.val "lang"
                                ]
                            )
                            (CG.apply [ CG.fqVal [ "Maybe" ] "Just", CG.val "l" ])
                            (CG.apply [ CG.val "helper", CG.val "ls" ])
                      )
                    ]
            ]
        <|
            CG.apply [ CG.val "helper", CG.parens <| CG.apply [ CG.fqFun [ "List" ] "reverse", CG.val names.languagesName ] ]
