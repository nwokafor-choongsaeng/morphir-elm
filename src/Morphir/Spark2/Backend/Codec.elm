module Morphir.Spark2.Backend.Codec exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Morphir.IR.FQName as FQName
import Morphir.IR.Path as Path
import Morphir.IR.Type.Codec as TypeCodec
import Morphir.Spark2.AST.Codec as ASTCodec
import Morphir.Spark2.Backend exposing (Error(..), Errors, Options)


decodeOptions : Decode.Decoder Options
decodeOptions =
    Decode.maybe
        (Decode.field "config"
            (Decode.maybe
                (Decode.list Decode.string
                    |> Decode.map (List.map Path.fromString)
                )
            )
        )
        |> Decode.map (Maybe.map Options >> Maybe.withDefault (Options Nothing))


encodeErrors : Errors -> Encode.Value
encodeErrors errors =
    Encode.list encodeError errors


encodeError : Error -> Encode.Value
encodeError err =
    case err of
        FunctionNotFound fQName ->
            Encode.list Encode.string
                [ "FunctionNotFound"
                , FQName.toString fQName
                ]

        UnknownArgumentType tpe ->
            Encode.list identity
                [ Encode.string "UnknownArgumentType"
                , TypeCodec.encodeType (always Encode.null) tpe
                ]

        MappingError fqn error ->
            Encode.list identity
                [ Encode.string "MappingError"
                , Encode.string (FQName.toString fqn)
                , ASTCodec.encodeError error
                ]
