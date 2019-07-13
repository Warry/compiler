module AST.Unwrapped.Canonical exposing
    ( Expr
    , unwrap
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import AST.Canonical as Canonical
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Dict.Any exposing (AnyDict)


type Expr
    = Literal Literal
    | Var { qualifier : ModuleName, name : VarName }
    | Argument VarName
    | Plus Expr Expr
    | Lambda { argument : VarName, body : Expr }
    | Call { fn : Expr, argument : Expr }
    | If { test : Expr, then_ : Expr, else_ : Expr }
    | Let { bindings : AnyDict String VarName (Binding Expr), body : Expr }
    | List (List Expr)
    | Unit


unwrap : Canonical.Expr -> Expr
unwrap expr =
    case Located.unwrap expr of
        Canonical.Literal literal ->
            Literal literal

        Canonical.Var var ->
            Var var

        Canonical.Argument var ->
            Argument var

        Canonical.Plus t1 t2 ->
            Plus (unwrap t1) (unwrap t2)

        Canonical.Lambda { argument, body } ->
            Lambda
                { argument = argument
                , body = unwrap body
                }

        Canonical.Call { fn, argument } ->
            Call
                { fn = unwrap fn
                , argument = unwrap argument
                }

        Canonical.If { test, then_, else_ } ->
            If
                { test = unwrap test
                , then_ = unwrap then_
                , else_ = unwrap else_
                }

        Canonical.Let { bindings, body } ->
            Let
                { bindings = unwrapBindings bindings
                , body = unwrap body
                }

        Canonical.List list ->
            List (List.map unwrap list)

        Canonical.Unit ->
            Unit


unwrapBindings : AnyDict String VarName (Binding Canonical.Expr) -> AnyDict String VarName (Binding Expr)
unwrapBindings =
    Debug.todo "unwrap bindings"
