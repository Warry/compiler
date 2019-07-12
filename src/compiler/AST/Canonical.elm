module AST.Canonical exposing
    ( Expr
    , Expr_(..)
    , ProjectFields
    , lambda
    , var
    )

import AST.Common.Literal exposing (Literal)
import AST.Common.Located as Located exposing (Located)
import Common.Types
    exposing
        ( Binding
        , ModuleName
        , Modules
        , VarName
        )
import Dict.Any exposing (AnyDict)


type alias ProjectFields =
    { modules : Modules Expr }


type alias Expr =
    Located Expr_


type Expr_
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


var : ModuleName -> VarName -> Expr_
var qualifier name =
    Var
        { qualifier = qualifier
        , name = name
        }


lambda : VarName -> Expr -> Expr_
lambda argument body =
    Lambda
        { argument = argument
        , body = body
        }
