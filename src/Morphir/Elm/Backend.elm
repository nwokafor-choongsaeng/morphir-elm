{-
   Copyright 2020 Morgan Stanley

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Morphir.Elm.Backend exposing (..)

import Dict exposing (Dict)
import Morphir.Elm.Backend.PrettyPrinter as PrettyPrinter exposing (Imports)
import Morphir.File.FileMap exposing (FileMap)
import Morphir.IR.AccessControlled exposing (AccessControlled)
import Morphir.IR.Distribution exposing (Distribution(..))
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package as Package exposing (PackageName)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value
import Set


mapDistribution : Distribution -> FileMap
mapDistribution distro =
    case distro of
        Library packageName _ definition ->
            mapModules packageName definition


pathToString : Path -> String
pathToString path =
    Path.toString Name.toTitleCase "." path


mapModules : PackageName -> Package.Definition () (Type ()) -> FileMap
mapModules packageName packageDef =
    packageDef.modules
        |> Dict.map
            (\modPath accessControlledModDef ->
                PrettyPrinter.mapModule packageName
                    modPath
                    (accessControlledModDef.value
                        |> collectImports packageName modPath
                    )
                    accessControlledModDef.value
            )
        |> Dict.foldl
            (\modPath modContent fileMapSoFar ->
                fileMapSoFar
                    |> Dict.insert (asFilePath packageName modPath) modContent
            )
            Dict.empty


collectImports : PackageName -> ModuleName -> Module.Definition () (Type ()) -> Imports
collectImports packageName moduleName modDef =
    Set.union
        (collectImportsFromType packageName moduleName modDef.types)
        (collectImportFromValues packageName moduleName modDef.values)


collectImportsFromType : PackageName -> ModuleName -> Dict Name (AccessControlled (Documented (Type.Definition ta))) -> Imports
collectImportsFromType packageName moduleName types =
    types
        |> Dict.foldl
            (\_ accCntrldDocTpeDef importsSoFar ->
                Type.collectReferencesFromDefintion accCntrldDocTpeDef.value.value
                    |> Set.filter (\( p, m, _ ) -> p /= packageName && m /= moduleName)
                    |> Set.map (fqnToPath >> pathToString)
                    |> Set.union importsSoFar
            )
            Set.empty


collectImportFromValues : PackageName -> ModuleName -> Dict Name (AccessControlled (Documented (Value.Definition ta va))) -> Imports
collectImportFromValues packageName moduleName values =
    values
        |> Dict.foldl
            (\_ accCntrldDocValueDef importsSoFar ->
                let
                    tpeImports : Imports
                    tpeImports =
                        accCntrldDocValueDef.value.value.inputTypes
                            |> List.map (\( _, _, tpe ) -> Type.collectReferences tpe)
                            |> List.foldl Set.union (Type.collectReferences accCntrldDocValueDef.value.value.outputType)
                            |> Set.filter (\( p, m, _ ) -> p /= packageName && m /= moduleName)
                            |> Set.map (fqnToPath >> pathToString)
                in
                Value.collectReferences accCntrldDocValueDef.value.value.body
                    |> Set.filter (\( p, m, _ ) -> p /= packageName && m /= moduleName)
                    |> Set.map (fqnToPath >> pathToString)
                    |> Set.union tpeImports
                    |> Set.union importsSoFar
            )
            Set.empty


fqnToPath : FQName -> Path
fqnToPath ( p, m, _ ) =
    p ++ m


asFilePath : Path -> Path -> ( List String, String )
asFilePath packagePath modulePath =
    let
        helper : Path -> List String -> ( List String, String )
        helper ls soFar =
            case ls of
                [] ->
                    ( soFar, "" )

                last :: [] ->
                    ( soFar, Name.toTitleCase last ++ ".elm" )

                name :: rest ->
                    helper rest (soFar ++ [ Name.toTitleCase name ])
    in
    helper (packagePath ++ modulePath) []
