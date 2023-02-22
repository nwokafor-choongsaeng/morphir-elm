module Morphir.Spark2.Backend exposing (..)

import Dict exposing (Dict)
import Morphir.File.FileMap exposing (FileMap)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.AccessControlled exposing (AccessControlled)
import Morphir.IR.Distribution exposing (Distribution(..))
import Morphir.IR.FQName as FQName exposing (FQName)
import Morphir.IR.Literal exposing (Literal(..))
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Package exposing (PackageName)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value exposing (TypedValue, Value)
import Morphir.SDK.ResultList as ResultList
import Morphir.Scala.AST as Scala
import Morphir.Scala.PrettyPrinter as PrettyPrinter
import Morphir.Spark.API as Spark
import Morphir.Spark2.AST as SparkAST
import Morphir.Spark2.ContextAPI as Context exposing (Context)


type alias Options =
    { modulesToProcess : Maybe (List ModuleName) }


type Op
    = Transformation
    | ColExpression
    | Action


type alias ValueAttr =
    { op : Op
    , tpe : Type ()
    }


type alias ValueDef =
    Value.Definition () (Type ())


type alias AnnotatedValueDef =
    Value.Definition () ValueAttr


type Error
    = FunctionNotFound FQName
    | UnknownArgumentType (Type ())
    | MappingError FQName SparkAST.Error


type alias Errors =
    List Error


hardcodedEntryPoints : List FQName
hardcodedEntryPoints =
    FQName.fqn "Foo" "Spark" "entryPoint"
        |> List.singleton


mapDistribution : Options -> Distribution -> Result Errors FileMap
mapDistribution opts distribution =
    case distribution of
        Library packageName _ packageDef ->
            let
                ir : IR
                ir =
                    IR.fromDistribution distribution

                modulesToProcess : Dict ModuleName (AccessControlled (Module.Definition () (Type ())))
                modulesToProcess =
                    case opts.modulesToProcess of
                        Just modNames ->
                            packageDef.modules
                                |> Dict.filter (\k _ -> List.member k modNames)

                        Nothing ->
                            packageDef.modules

                _ =
                    Debug.log "INFO:" ("Processing " ++ (Dict.size modulesToProcess |> String.fromInt) ++ " modules")

                entryPoints : List ( FQName, ValueDef )
                entryPoints =
                    packageDef.modules
                        |> Dict.foldl
                            (\modPath modDef lstSoFar ->
                                modDef.value.values
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( localName, v ) ->
                                            if List.member ( packageName, modPath, localName ) hardcodedEntryPoints then
                                                Just ( ( packageName, modPath, localName ), v.value.value )

                                            else
                                                Nothing
                                        )
                                    |> (\lst -> lst ++ lstSoFar)
                            )
                            []

                context : Context
                context =
                    getContext ir entryPoints

                --annotatedEntryPoints =
                --    annotateEntryPoints ir context entryPoints
                fileMap : FileMap
                fileMap =
                    Dict.empty

                --_ =
                --    Debug.log "context" context
            in
            modulesToProcess
                |> Dict.toList
                |> List.map
                    (\( moduleName, accessControlledModuleDef ) ->
                        let
                            packagePath : List String
                            packagePath =
                                packageName
                                    ++ moduleName
                                    |> List.map (Name.toCamelCase >> String.toLower)

                            objectResult : Result Error Scala.TypeDecl
                            objectResult =
                                accessControlledModuleDef.value.values
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( valueName, accCntrldDcmntedValueDef ) ->
                                            case
                                                mapFunctionDefinition ir ( packageName, moduleName, valueName ) accCntrldDcmntedValueDef.value.value
                                                    |> Result.map Scala.withoutAnnotation
                                            of
                                                Ok v ->
                                                    Just (Ok v)

                                                Err e ->
                                                    let
                                                        _ =
                                                            if moduleName == [ [ "antique", "rules", "tests" ] ] then
                                                                Debug.log "\n\n\nFailed while mapping" (Debug.toString e)

                                                            else
                                                                Debug.log "" ""
                                                    in
                                                    Nothing
                                        )
                                    --|> ResultList.partition
                                    --    (\( errs, vals ) ->
                                    --        Ok <|
                                    --            Scala.Object
                                    --                { modifiers = []
                                    --                , name = "SparkJobs"
                                    --                , extends = []
                                    --                , members = vals
                                    --                , body = Nothing
                                    --                }
                                    --    )
                                    |> ResultList.keepFirstError
                                    |> Result.map
                                        (\memberDeclrs ->
                                            Scala.Object
                                                { modifiers = []
                                                , name = "SparkJobs"
                                                , extends = []
                                                , members = memberDeclrs
                                                , body = Nothing
                                                }
                                        )

                            compilationUnitResult : Result Error Scala.CompilationUnit
                            compilationUnitResult =
                                objectResult
                                    |> Result.map
                                        (\object ->
                                            { dirPath = packagePath
                                            , fileName = "SparkJobs.scala"
                                            , packageDecl = packagePath
                                            , imports = []
                                            , typeDecls = [ Scala.Documented Nothing (Scala.withoutAnnotation object) ]
                                            }
                                        )
                        in
                        compilationUnitResult
                            |> Result.map
                                (\compilationUnit ->
                                    ( ( packagePath, "SparkJobs.scala" )
                                    , PrettyPrinter.mapCompilationUnit (PrettyPrinter.Options 2 80) compilationUnit
                                    )
                                )
                    )
                |> ResultList.keepAllErrors
                |> Result.map Dict.fromList


{-| Maps function definitions defined within the current package to scala
-}
mapFunctionDefinition : IR -> FQName -> Value.Definition () (Type ()) -> Result Error Scala.MemberDecl
mapFunctionDefinition ir (( _, _, localFunctionName ) as fqName) valueDefinition =
    let
        mapFunctionInputs : List ( Name, va, Type () ) -> Result Error (List Scala.ArgDecl)
        mapFunctionInputs inputTypes =
            inputTypes
                |> List.map
                    (\( argName, _, argType ) ->
                        case argType of
                            Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ _ ] ->
                                Ok
                                    { modifiers = []
                                    , tpe = Spark.dataFrame
                                    , name = Name.toCamelCase argName
                                    , defaultValue = Nothing
                                    }

                            other ->
                                Err (UnknownArgumentType other)
                    )
                |> ResultList.keepFirstError
    in
    Result.map2
        (\scalaArgs scalaFunctionBody ->
            Scala.FunctionDecl
                { modifiers = []
                , name = localFunctionName |> Name.toCamelCase
                , typeArgs = []
                , args = [ scalaArgs ]
                , returnType = Just Spark.dataFrame
                , body = Just scalaFunctionBody
                }
        )
        (mapFunctionInputs valueDefinition.inputTypes)
        (mapValue ir fqName valueDefinition.body)


{-| Maps morphir values to scala values
-}
mapValue : IR -> FQName -> TypedValue -> Result Error Scala.Value
mapValue ir fqn body =
    body
        |> SparkAST.objectExpressionFromValue ir
        |> Result.mapError (MappingError fqn)
        |> Result.andThen mapObjectExpressionToScala


{-| Maps Spark ObjectExpressions to scala values.
ObjectExpressions are defined as part of the SparkIR
-}
mapObjectExpressionToScala : SparkAST.ObjectExpression -> Result Error Scala.Value
mapObjectExpressionToScala objectExpression =
    case objectExpression of
        SparkAST.From name ->
            Name.toCamelCase name |> Scala.Variable |> Ok

        SparkAST.Filter predicate sourceRelation ->
            mapObjectExpressionToScala sourceRelation
                |> Result.map
                    (Spark.filter
                        (mapExpression predicate)
                    )

        SparkAST.Select fieldExpressions sourceRelation ->
            mapObjectExpressionToScala sourceRelation
                |> Result.map
                    (Spark.select
                        (fieldExpressions
                            |> mapNamedExpressions
                        )
                    )

        SparkAST.Aggregate groupfield fieldExpressions sourceRelation ->
            mapObjectExpressionToScala sourceRelation
                |> Result.map
                    (Spark.aggregate
                        groupfield
                        (mapNamedExpressions fieldExpressions)
                    )

        SparkAST.Join joinType baseRelation joinedRelation onClause ->
            let
                joinTypeName : String
                joinTypeName =
                    case joinType of
                        SparkAST.Inner ->
                            "inner"

                        SparkAST.Left ->
                            "left"
            in
            Result.map2
                (\baseDataFrame joinedDataFrame ->
                    Spark.join
                        baseDataFrame
                        (mapExpression onClause)
                        joinTypeName
                        joinedDataFrame
                )
                (mapObjectExpressionToScala baseRelation)
                (mapObjectExpressionToScala joinedRelation)


{-| Maps Spark Expressions to scala values.
Expressions are defined as part of the SparkIR.
-}
mapExpression : SparkAST.Expression -> Scala.Value
mapExpression expression =
    case expression of
        SparkAST.BinaryOperation simpleExpression leftExpr rightExpr ->
            Scala.BinOp
                (mapExpression leftExpr)
                simpleExpression
                (mapExpression rightExpr)

        SparkAST.Column colName ->
            Spark.column colName

        SparkAST.Literal literal ->
            mapLiteral literal |> Scala.Literal

        SparkAST.Variable name ->
            Scala.Variable name

        SparkAST.Not expr ->
            Scala.Apply
                (Scala.Ref [ "org", "apache", "spark", "sql", "functions" ] "not")
                (mapExpression expr |> Scala.ArgValue Nothing |> List.singleton)

        SparkAST.WhenOtherwise condition thenBranch elseBranch ->
            let
                toIfElseChain : SparkAST.Expression -> Scala.Value -> Scala.Value
                toIfElseChain v branchesSoFar =
                    case v of
                        SparkAST.WhenOtherwise cond nextThenBranch nextElseBranch ->
                            Spark.andWhen
                                (mapExpression cond)
                                (mapExpression nextThenBranch)
                                branchesSoFar
                                |> toIfElseChain nextElseBranch

                        _ ->
                            Spark.otherwise
                                (mapExpression v)
                                branchesSoFar
            in
            Spark.when
                (mapExpression condition)
                (mapExpression thenBranch)
                |> toIfElseChain elseBranch

        SparkAST.Method target name argList ->
            Scala.Apply
                (Scala.Select (mapExpression target) name)
                (argList
                    |> List.map mapExpression
                    |> List.map (Scala.ArgValue Nothing)
                )

        SparkAST.Function name argList ->
            Scala.Apply
                (Scala.Ref [ "org", "apache", "spark", "sql", "functions" ] name)
                (argList
                    |> List.map mapExpression
                    |> List.map (Scala.ArgValue Nothing)
                )


{-| Maps NamedExpressions to scala values.
-}
mapNamedExpressions : SparkAST.NamedExpressions -> List Scala.Value
mapNamedExpressions nameExpressions =
    List.map
        (\( columnName, named ) ->
            mapExpression named
                |> Spark.alias (Name.toCamelCase columnName)
        )
        nameExpressions


{-| Maps Spark Literals to scala Literals.
-}
mapLiteral : Literal -> Scala.Lit
mapLiteral l =
    case l of
        BoolLiteral bool ->
            Scala.BooleanLit bool

        CharLiteral char ->
            Scala.CharacterLit char

        StringLiteral string ->
            Scala.StringLit string

        WholeNumberLiteral int ->
            Scala.IntegerLit int

        FloatLiteral float ->
            Scala.FloatLit float

        DecimalLiteral _ ->
            Debug.todo "branch 'DecimalLiteral _' not implemented"


getContext : IR -> List ( FQName, ValueDef ) -> Context
getContext ir entryPoints =
    entryPoints
        |> List.foldl
            (\( _, valDef ) ->
                Context.extendFromValueDef ir valDef
                    >> Context.extendFromType ir Nothing valDef.outputType
            )
            Context.empty


annotateEntryPoints : IR -> Context -> List ( FQName, ValueDef ) -> List ( FQName, AnnotatedValueDef )
annotateEntryPoints ir context entryPoints =
    Debug.todo ""
