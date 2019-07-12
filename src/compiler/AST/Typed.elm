module AST.Typed exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , isArgument
    , lambda
    , let_
    , recursiveChildren
    , transformAll
    , transformOnce
    , mapTyped
    , getExpr
    , getType
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import AST.Common.Type exposing (Type)
import Common
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Dict.Any exposing (AnyDict)
import Transform


type alias ProjectFields =
    { modules : Modules Expr }


{-| Differs from Canonical.Expr by:

  - being a tuple of the underlying Expr\_ type and its type

TODO make this opaque, add accessors etc.

-}
type alias Expr =
    Located Typed


type alias Typed =
    ( Expr_, Type )


type Expr_
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda
        { argument : VarName
        , body : Expr
        }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : AnyDict String VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit


lambda : VarName -> Expr -> Expr_
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }


let_ : AnyDict String VarName (Binding Expr) -> Expr -> Expr_
let_ bindings body =
    Let
        { bindings = bindings
        , body = body
        }


{-| A helper for the Transform library.
-}
recurse : (Typed -> Typed) -> Typed -> Typed
recurse f (expr_, type_) =
    ( case expr_ of
        Literal _ ->
            expr_

        Var _ ->
            expr_

        Argument _ ->
            expr_

        Plus e1 e2 ->
            Plus
                (Located.map f e1)
                (Located.map f e2)

        Lambda ({ body } as lambda_) ->
            Lambda { lambda_ | body = Located.map f body }

        Call { fn, argument } ->
            Call
                { fn = Located.map f fn
                , argument = Located.map f argument
                }

        If { test, then_, else_ } ->
            If
                { test = Located.map f test
                , then_ = Located.map f then_
                , else_ = Located.map f else_
                }

        Let { bindings, body } ->
            Let
                { bindings = Dict.Any.map (always (Common.mapBinding (Located.map f))) bindings
                , body = Located.map f body
                }

        List items ->
            List (List.map (Located.map f) items)

        Unit ->
            expr_
    , type_
    )


transformOnce : (Typed -> Typed) -> Typed -> Typed
transformOnce pass expr_ =
    Transform.transformOnce
        recurse
        pass
        expr_


transformAll : List (Typed -> Maybe Typed) -> Typed -> Typed
transformAll passes expr_ =
    Transform.transformAll
        recurse
        (Transform.orList passes)
        expr_


isArgument : VarName -> Expr -> Bool
isArgument name expr =
    case getExpr expr of
        Argument argName ->
            argName == name

        _ ->
            False


recursiveChildren : (Expr -> List Expr) -> Expr -> List Expr
recursiveChildren fn expr =
    let
        expr_ = getExpr expr
    in
    case expr_ of
        Literal _ ->
            []

        Var _ ->
            []

        Argument _ ->
            []

        Plus left right ->
            fn left
                ++ fn right

        Lambda { body } ->
            fn body

        Call data ->
            fn data.fn
                ++ fn data.argument

        If { test, then_, else_ } ->
            fn test
                ++ fn then_
                ++ fn else_

        Let { bindings, body } ->
            fn body
                ++ List.concatMap (.body >> fn) (Dict.Any.values bindings)

        Unit ->
            []

        List items ->
            List.concatMap fn items


mapTyped : (a -> b) -> Located ( a, c ) -> Located ( b, c )
mapTyped =
    Located.map << Tuple.mapFirst


getExpr : Located ( a, b ) -> a
getExpr =
    Tuple.first << Located.unwrap


getType : Located ( a, b ) -> b
getType =
    Tuple.second << Located.unwrap

