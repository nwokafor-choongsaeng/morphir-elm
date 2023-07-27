module Morphir.Elm.Backend.PrettyPrinter exposing (..)

import Dict
import Morphir.File.SourceCode as Doc exposing (Doc)
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName as FQName
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Type as Type exposing (Type)
import Set exposing (Set)


type alias Imports =
    Set String


{-| One Elm indent is equivalent to four spaces.
-}
type alias Depth =
    Int


type alias MultiLine =
    Bool


baseDepth : Depth
baseDepth =
    4


nextDepth : Depth -> Depth
nextDepth currentDepth =
    currentDepth + baseDepth


mapModule : PackageName -> ModuleName -> Imports -> Module.Definition () (Type ()) -> Doc
mapModule packageName modName imports definition =
    let
        moduleDecl =
            -- exposes all types and values
            String.join Doc.space
                [ "module"
                , packageName
                    ++ modName
                    |> Path.toString Name.toTitleCase "."
                , "exposing (..)"
                ]

        importsDoc =
            Set.toList imports
                |> List.map (\modulePath -> "import " ++ modulePath)
                |> String.join Doc.newLine

        typesDoc =
            definition.types
                |> Dict.toList
                |> List.map
                    (\( tpeName, accessControlledDocumented ) ->
                        mapTypeDefinition packageName
                            modName
                            tpeName
                            accessControlledDocumented.value
                    )
                |> String.join "\n\n\n"

        valuesDoc =
            Doc.empty
    in
    String.join "\n\n\n"
        [ moduleDecl
        , importsDoc
        , typesDoc
        , valuesDoc
        ]


mapTypeDefinition : PackageName -> ModuleName -> Name -> Documented (Type.Definition ()) -> Doc
mapTypeDefinition packageName modName name documentedTypeDef =
    case documentedTypeDef.value of
        Type.TypeAliasDefinition tpeParams tpe ->
            String.join Doc.space [ "type alias", Name.toTitleCase name ]
                ++ String.concat
                    [ List.map Name.toCamelCase tpeParams
                        |> String.join ""
                    , " ="
                    ]
                ++ Doc.newLine
                ++ Doc.indent baseDepth (mapType packageName modName tpe)

        Type.CustomTypeDefinition tpeParams { value } ->
            String.join Doc.space
                [ "type"
                , Name.toTitleCase name
                , List.map Name.toCamelCase tpeParams
                    |> String.join Doc.space
                ]
                ++ Doc.newLine
                ++ ("= "
                        ++ (Dict.toList value
                                |> List.map
                                    (\( ctorName, ctorArgs ) ->
                                        String.join Doc.space
                                            [ Name.toTitleCase ctorName
                                            , ctorArgs
                                                |> List.map
                                                    (Tuple.second
                                                        >> mapType packageName modName
                                                        >> Doc.parens
                                                    )
                                                |> String.join Doc.space
                                            ]
                                    )
                                |> String.join "\n| "
                           )
                        |> Doc.indent baseDepth
                   )


mapType : PackageName -> ModuleName -> Type () -> Doc
mapType packageName moduleName tpe =
    case tpe of
        Type.Variable () name ->
            Name.toCamelCase name

        Type.Reference () ( pkgName, modName, localName ) typeArgs ->
            let
                tpeArgsDoc =
                    if List.length typeArgs > 0 then
                        " "
                            ++ String.join Doc.space
                                (List.map (mapType packageName modName >> Doc.parens) typeArgs)

                    else
                        ""
            in
            if pkgName == packageName && modName == moduleName then
                -- reference within the same module doesn't need qualification
                Name.toTitleCase localName ++ tpeArgsDoc

            else
                String.concat
                    [ ((pkgName ++ modName) |> List.map Name.toTitleCase)
                        ++ [ Name.toTitleCase localName ]
                        |> Doc.dotSep
                    , tpeArgsDoc
                    ]

        Type.Tuple () types ->
            String.join ", "
                (types
                    |> List.map (mapType packageName moduleName)
                )
                |> Doc.parens

        Type.Record () fields ->
            Doc.concat
                [ "{ "
                , String.join ", "
                    (List.map
                        (\field ->
                            Name.toCamelCase field.name
                                ++ " : "
                                ++ mapType packageName moduleName field.tpe
                        )
                        fields
                    )
                , " }"
                ]

        Type.ExtensibleRecord () name fields ->
            Doc.concat
                [ "{ "
                , Name.toCamelCase name
                , " | "
                , String.join ", "
                    (List.map
                        (\field ->
                            Name.toCamelCase field.name
                                ++ " : "
                                ++ mapType packageName moduleName field.tpe
                        )
                        fields
                    )
                , " }"
                ]

        Type.Function () inputTpe outputType ->
            String.join " -> "
                [ mapType packageName moduleName inputTpe
                , mapType packageName moduleName outputType
                ]

        Type.Unit () ->
            "()"
