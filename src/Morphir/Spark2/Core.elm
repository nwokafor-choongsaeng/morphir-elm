module Morphir.Spark2.Core exposing (..)

--import AssocList exposing (Dict)

import Morphir.IR.Distribution as Morphir
import Morphir.IR.FQName as Morphir
import Morphir.IR.Type as Morphir


{-| A representation of `morphir.sql.types`.
-}
type Type
    = String
    | Struct
    | Long
    | Char
    | Double
    | Boolean
    | Array Type
    | Map Type Type
    | Struct Fields


type DataFrame
    = DataFrame Type


type alias Fields =
    Dict Name ( Type, Nullable )


type alias Name =
    String


type alias Nullable =
    Bool


mapMorphirTypeToSparkType : Morphir.Type () -> Type
mapMorphirTypeToSparkType morphirTpe =
    case morphirTpe of
        Morphir.Variable a name ->
            Debug.todo "Implement this"

        Morphir.Reference a fQName types ->
            case fQName of
                ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "string" ] ], [ "string" ] ) ->
                    String

                ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "int" ] ) ->
                    Long

                ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "float" ] ) ->
                    Double

                ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) ->
                    Boolean

                ( [ [ "morphir" ], [ "s", "d", "k" ] ], [ [ "basics" ] ], [ "bool" ] ) ->
                    Boolean

                _ ->
                    Debug.todo "Implement this"

        Morphir.Tuple a types ->
            Debug.todo "Implement this"

        Morphir.Record a fields ->
            Debug.todo "Implement this"

        Morphir.ExtensibleRecord a name fields ->
            Debug.todo "Implement this"

        Morphir.Function a tpe1 tpe2 ->
            Debug.todo "Implement this"

        Morphir.Unit a ->
            Debug.todo "Implement this"


identifyAndCollectDataFrames : Morphir.Distribution -> Dict Morphir.FQName DataFrame
identifyAndCollectDataFrames distro =
    case distro of
        Morphir.Library packageName dict definition ->
            Debug.todo "Implement this"
