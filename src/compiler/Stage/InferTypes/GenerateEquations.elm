module Stage.InferTypes.GenerateEquations exposing (generateEquations)

{-| Stage 2

We try to bind various subexpressions together here.
For example consider this:

    42

This is an Integer (or a `number`, we don't care here for the sake of simplicity).

    foo 42

What can we say about `foo`? It's a function from Integer (as evidenced by 42),
but we don't know to what. Right now the best we could say is `foo : Int -> a`.

    foo 42 == "abc"

Suddenly everything's clear: the whole thing is Bool because of `==`, and thanks
to the right side of `==` being a String and the two sides having to match in types,
we know `foo 42` is a String too, and thus `foo : Int -> String`.

The "two sides of `==` have to match" insight could be expressed here as something like

    equals leftType rightType

The goal of this module is to build the list of such equations for each
subexpression.

-}

import AST.Common.Literal as Literal
import AST.Common.Type as Type exposing (Type)
import AST.Typed as Typed
import Common.Types exposing (VarName)
import Dict.Any
import Stage.InferTypes.IdSource as IdSource exposing (IdSource)
import Stage.InferTypes.TypeEquation exposing (TypeEquation, equals)
import Transform


generateEquations : IdSource -> Typed.Expr -> ( List TypeEquation, IdSource )
generateEquations idSource ( expr, type_ ) =
    case expr of
        Typed.Literal (Literal.Int _) ->
            -- integer is an integer ¯\_(ツ)_/¯
            ( [ equals type_ Type.Int ]
            , idSource
            )

        Typed.Literal (Literal.Float _) ->
            -- float is a float
            ( [ equals type_ Type.Float ]
            , idSource
            )

        Typed.Literal (Literal.Char _) ->
            -- char is a char
            ( [ equals type_ Type.Char ]
            , idSource
            )

        Typed.Literal (Literal.String _) ->
            -- string is a string
            ( [ equals type_ Type.String ]
            , idSource
            )

        Typed.Literal (Literal.Bool _) ->
            -- bool is a bool
            ( [ equals type_ Type.Bool ]
            , idSource
            )

        Typed.Argument _ ->
            -- we can't make any assumptions here
            ( [], idSource )

        Typed.Var _ ->
            -- we can't make any assumptions here
            ( [], idSource )

        Typed.Plus left right ->
            let
                ( _, leftType ) =
                    left

                ( _, rightType ) =
                    right

                ( leftEquations, idSource1 ) =
                    generateEquations idSource left

                ( rightEquations, idSource2 ) =
                    generateEquations idSource1 right
            in
            ( -- for expression `a + b`:
              [ equals leftType Type.Int -- type of `a` is Int
              , equals rightType Type.Int -- type of `b` is Int
              , equals type_ Type.Int -- type of `a + b` is Int
              ]
                ++ leftEquations
                ++ rightEquations
            , idSource2
            )

        Typed.Lambda { body, argument } ->
            let
                ( bodyExpr, bodyType ) =
                    body

                ( argumentId, idSource1 ) =
                    IdSource.increment idSource

                ( bodyEquations, idSource2 ) =
                    generateEquations idSource1 body

                usages =
                    findArgumentUsages argument body

                usageEquations =
                    generateArgumentUsageEquations argumentId usages
            in
            ( -- type of `\arg -> body` is (arg -> body)
              equals type_ (Type.Function (Type.Var argumentId) bodyType)
                -- type of the argument is the same as the type of all the children usages of that argument
                :: usageEquations
                ++ bodyEquations
            , idSource2
            )

        Typed.Call { fn, argument } ->
            let
                ( _, fnType ) =
                    fn

                ( _, argumentType ) =
                    argument

                ( fnEquations, idSource1 ) =
                    generateEquations idSource fn

                ( argumentEquations, idSource2 ) =
                    generateEquations idSource1 argument
            in
            ( -- for expression `a b`:
              -- type of `a` is (argumentType -> resultType)
              equals fnType (Type.Function argumentType type_)
                :: fnEquations
                ++ argumentEquations
            , idSource2
            )

        Typed.If { test, then_, else_ } ->
            let
                ( _, testType ) =
                    test

                ( _, thenType ) =
                    then_

                ( _, elseType ) =
                    else_

                ( testEquations, idSource1 ) =
                    generateEquations idSource test

                ( thenEquations, idSource2 ) =
                    generateEquations idSource1 then_

                ( elseEquations, idSource3 ) =
                    generateEquations idSource2 else_
            in
            ( -- for expression `if a then b else c`:
              [ equals testType Type.Bool -- type of `a` is Bool
              , equals thenType elseType -- types of `b` and `c` are the same
              , equals thenType type_ -- types of `b` and `if a then b else c` are the same
              ]
                ++ testEquations
                ++ thenEquations
                ++ elseEquations
            , idSource3
            )

        Typed.Let { bindings, body } ->
            let
                ( _, bodyType ) =
                    body

                ( bodyEquations, idSource1 ) =
                    generateEquations idSource body

                ( bindingEquations, idSource2 ) =
                    List.foldl
                        (\binding ( acc, currentIdSource ) ->
                            let
                                ( equations, nextIdSource ) =
                                    generateEquations currentIdSource binding.body
                            in
                            ( equations ++ acc
                            , nextIdSource
                            )
                        )
                        ( [], idSource1 )
                        (Dict.Any.values bindings)
            in
            ( -- for expression `let x = a, y = b in c` (pardon the comma):
              -- type of the whole let and type of `c` are the same
              equals bodyType type_
                :: bodyEquations
                ++ bindingEquations
            , idSource2
            )

        Typed.Unit ->
            -- unit is unit
            ( [ equals type_ Type.Unit ]
            , idSource
            )


findArgumentUsages : VarName -> Typed.Expr -> List Typed.Expr
findArgumentUsages argument bodyExpr =
    bodyExpr
        |> Transform.children Typed.recursiveChildren
        |> List.filter (Typed.isArgument argument)


generateArgumentUsageEquations : Int -> List Typed.Expr -> List TypeEquation
generateArgumentUsageEquations argumentId usages =
    let
        argumentType =
            Type.Var argumentId
    in
    List.map
        (\( _, usageType ) -> equals usageType argumentType)
        usages