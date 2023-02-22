module Morphir.Spark2.ContextAPI exposing (..)

import Dict exposing (Dict)
import Morphir.IR as IR exposing (IR)
import Morphir.IR.FQName exposing (FQName)
import Morphir.IR.Name exposing (Name)
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value
import Set exposing (Set)


type alias FieldName =
    Name


type alias TableDefinition =
    Dict FieldName (Type ())


type alias Id =
    Int


type alias Context =
    { tableDefinitions : Dict FQName TableDefinition
    , anonymousTableDefinitions : List ( Id, TableDefinition )
    }


empty : Context
empty =
    { tableDefinitions = Dict.empty
    , anonymousTableDefinitions = []
    }


extendFromValueDef : IR -> Value.Definition () (Type ()) -> Context -> Context
extendFromValueDef ir valDef initialContext =
    valDef.inputTypes
        |> List.foldl
            (\( _, _, tpe ) contextSoFar ->
                extendFromType ir Nothing tpe contextSoFar
            )
            initialContext


extendFromType : IR -> Maybe FQName -> Type () -> Context -> Context
extendFromType ir maybeFQName tpe initialContext =
    case tpe of
        Type.Record _ fields ->
            if isTableDefinition fields then
                tableDefinitionFromFields fields
                    |> Maybe.map
                        (\tableDef ->
                            case maybeFQName of
                                Just fqn ->
                                    { initialContext
                                        | tableDefinitions =
                                            Dict.insert fqn tableDef initialContext.tableDefinitions
                                    }

                                Nothing ->
                                    { initialContext
                                        | anonymousTableDefinitions =
                                            ( List.length initialContext.anonymousTableDefinitions + 1, tableDef )
                                                :: initialContext.anonymousTableDefinitions
                                    }
                        )
                    |> Maybe.withDefault initialContext

            else
                initialContext

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ Type.Record _ fields ] ->
            tableDefinitionFromFields fields
                |> Maybe.map
                    (\tableDef ->
                        { initialContext
                            | anonymousTableDefinitions =
                                ( List.length initialContext.anonymousTableDefinitions + 1, tableDef )
                                    :: initialContext.anonymousTableDefinitions
                        }
                    )
                |> Maybe.withDefault initialContext

        Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ Type.Reference _ tableRef [] ] ->
            case IR.lookupTypeSpecification tableRef ir of
                Just typeSpec ->
                    case typeSpec of
                        Type.TypeAliasSpecification [] (Type.Record _ fields) ->
                            case tableDefinitionFromFields fields of
                                Just tableDefs ->
                                    { initialContext
                                        | tableDefinitions =
                                            initialContext.tableDefinitions
                                                |> Dict.insert tableRef tableDefs
                                    }

                                Nothing ->
                                    initialContext

                        _ ->
                            initialContext

                Nothing ->
                    initialContext

        Type.Reference _ fQName [] ->
            case IR.lookupTypeSpecification fQName ir of
                Just typeSpec ->
                    case typeSpec of
                        Type.TypeAliasSpecification [] (Type.Record _ fields) ->
                            if isTableDefinition fields then
                                case tableDefinitionFromFields fields of
                                    Just tableDefs ->
                                        { initialContext
                                            | tableDefinitions =
                                                initialContext.tableDefinitions
                                                    |> Dict.insert fQName tableDefs
                                        }

                                    Nothing ->
                                        initialContext

                            else
                                initialContext

                        Type.TypeAliasSpecification [] (Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ tableRef ]) ->
                            extendFromType ir Nothing tableRef initialContext

                        _ ->
                            initialContext

                Nothing ->
                    initialContext

        _ ->
            initialContext


tableDefinitionFromFields : List (Type.Field ()) -> Maybe TableDefinition
tableDefinitionFromFields fields =
    fields
        |> List.foldl
            (\{ name, tpe } ->
                Maybe.andThen
                    (\dictSoFar ->
                        case tpe of
                            Type.Record _ _ ->
                                Nothing

                            Type.Variable _ _ ->
                                Nothing

                            Type.ExtensibleRecord _ _ _ ->
                                Nothing

                            Type.Function _ _ _ ->
                                Nothing

                            Type.Unit _ ->
                                Nothing

                            _ ->
                                Just (Dict.insert name tpe dictSoFar)
                    )
            )
            (Just Dict.empty)


sourcesFromFields : IR -> List (Type.Field ()) -> Maybe (Set FQName)
sourcesFromFields ir fields =
    fields
        |> List.foldl
            (\{ name, tpe } ->
                Maybe.andThen
                    (\setSoFar ->
                        case tpe of
                            Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ Type.Reference _ tableRef [] ] ->
                                case IR.lookupTypeSpecification tableRef ir of
                                    Just typeSpec ->
                                        case typeSpec of
                                            Type.TypeAliasSpecification [] (Type.Record _ innerFields) ->
                                                if isTableDefinition innerFields then
                                                    Just (Set.insert tableRef setSoFar)

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing

                                    Nothing ->
                                        Nothing

                            _ ->
                                Nothing
                    )
            )
            (Just Set.empty)


isSourceDefintion : IR -> List (Type.Field ()) -> Bool
isSourceDefintion ir fields =
    fields
        |> List.all
            (\{ name, tpe } ->
                case tpe of
                    Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ Type.Record _ innerFields ] ->
                        isTableDefinition innerFields

                    Type.Reference _ ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "list" ] ], [ "list" ] ) [ Type.Reference _ tableRef [] ] ->
                        ir
                            |> IR.lookupTypeSpecification tableRef
                            |> Maybe.map
                                (\tpeSpec ->
                                    case tpeSpec of
                                        Type.TypeAliasSpecification [] (Type.Record _ innerFields) ->
                                            isTableDefinition innerFields

                                        _ ->
                                            False
                                )
                            |> Maybe.withDefault False

                    Type.Record _ _ ->
                        False

                    Type.Variable _ _ ->
                        False

                    Type.ExtensibleRecord _ _ _ ->
                        False

                    Type.Function _ _ _ ->
                        False

                    Type.Unit _ ->
                        False

                    _ ->
                        True
            )


isTableDefinition : List (Type.Field ()) -> Bool
isTableDefinition fields =
    fields
        |> List.all
            (\{ name, tpe } ->
                case tpe of
                    Type.Record _ _ ->
                        False

                    Type.Variable _ _ ->
                        False

                    Type.ExtensibleRecord _ _ _ ->
                        False

                    Type.Function _ _ _ ->
                        False

                    Type.Unit _ ->
                        False

                    _ ->
                        True
            )
