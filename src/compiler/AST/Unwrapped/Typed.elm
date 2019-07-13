module AST.Unwrapped.Typed exposing
    ( Expr
    , unwrap
    )

import AST.Typed as Typed 
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


type alias Expr =
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


unwrap : Typed.Expr -> Expr
unwrap expr =
    let
        (expr_, type_) = Located.unwrap expr
    in
        ( case expr_ of
            Typed.Literal literal ->
                Literal literal

            Typed.Var var ->
                Var var

            Typed.Argument var ->
                Argument var

            Typed.Plus t1 t2 ->
                Plus (unwrap t1) (unwrap t2)

            Typed.Lambda { argument, body } ->
                Lambda
                    { argument = argument
                    , body = unwrap body
                    }

            Typed.Call { fn, argument } ->
                Call
                    { fn = unwrap fn
                    , argument = unwrap argument
                    }

            Typed.If { test, then_, else_ } ->
                If
                    { test = unwrap test
                    , then_ = unwrap then_
                    , else_ = unwrap else_
                    }

            Typed.Let { bindings, body } ->
                Let
                    { bindings = unwrapBindings bindings
                    , body = unwrap body
                    }

            Typed.List list ->
                List (List.map unwrap list)

            Typed.Unit ->
                Unit
    , type_
    )

unwrapBindings : AnyDict String VarName (Binding Typed.Expr) -> AnyDict String VarName (Binding Expr)
unwrapBindings =
    Debug.todo "unwrap bindings"
