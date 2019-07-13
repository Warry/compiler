module AST.Unwrapped.Frontend exposing
    ( Expr
    , unwrap
    )

import AST.Frontend as Frontend
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


type Expr
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


unwrap : Frontend.Expr -> Expr
unwrap expr =
    case Located.unwrap expr of
        Frontend.Literal literal ->
            Literal literal

        Frontend.Var var ->
            Var var

        Frontend.Argument var ->
            Argument var

        Frontend.Plus t1 t2 ->
            Plus (unwrap t1) (unwrap t2)

        Frontend.Lambda { arguments, body } ->
            Lambda
                { arguments = arguments
                , body = unwrap body
                }

        Frontend.Call { fn, argument } ->
            Call
                { fn = unwrap fn
                , argument = unwrap argument
                }

        Frontend.If { test, then_, else_ } ->
            If
                { test = unwrap test
                , then_ = unwrap then_
                , else_ = unwrap else_
                }

        Frontend.Let { bindings, body } ->
            Let
                { bindings = unwrapBindings bindings
                , body = unwrap body
                }

        Frontend.List list ->
            List (List.map unwrap list)

        Frontend.Unit ->
            Unit


unwrapBindings : List (Binding Frontend.Expr) -> List (Binding Expr)
unwrapBindings =
    Debug.todo "unwrap bindings"
