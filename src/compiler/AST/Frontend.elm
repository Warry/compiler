module AST.Frontend exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , lambda
    , transform
    , var
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import Common
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Transform


type alias ProjectFields =
    { modules : Modules Expr }


type alias Expr =
    Located Expr_


type Expr_
    = Literal Literal
    | Var { qualifier : Maybe ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda { arguments : List VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : List (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit


var : Maybe ModuleName -> VarName -> Expr_
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : List VarName -> Expr -> Expr_
lambda arguments body =
    Lambda
        { arguments = arguments
        , body = body
        }



{- Let's not get ahead of ourselves

   | Let
       { varName : VarName
       , varBody : Expr
       , body : Expr
       }
   | Fixpoint Expr
   | Operator
       { opName : VarName
       , left : Expr
       , right : Expr
       }
-}


{-| A helper for the Transform library.
-}
recurse : (Expr_ -> Expr_) -> Expr_ -> Expr_
recurse f expr =
    case expr of
        Literal _ ->
            expr

        Var _ ->
            expr

        Argument _ ->
            expr

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
                { bindings = List.map (Common.mapBinding (Located.map f)) bindings
                , body = Located.map f body
                }

        List items ->
            List (List.map (Located.map f) items)

        Unit ->
            expr


transform : (Expr_ -> Expr_) -> Expr_ -> Expr_
transform pass expr =
    {- If we do more than one at the same time, we should use the Transform
       library a bit differently, see `Transform.orList`.
    -}
    Transform.transformAll
        recurse
        (Transform.toMaybe pass)
        expr
