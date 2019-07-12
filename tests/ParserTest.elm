module ParserTest exposing
    ( dependencies
    , exposingList
    , expr
    , moduleDeclaration
    , moduleName
    )

import AST.Common.Literal exposing (Literal(..))
import AST.Common.Located exposing (Located(..), located)
import AST.Frontend exposing (Expr, Expr_(..))
import Common
import Common.Types
    exposing
        ( ExposedItem(..)
        , Exposing(..)
        , ModuleName(..)
        , ModuleType(..)
        , VarName(..)
        )
import Dict.Any
import Error exposing (ParseContext, ParseProblem)
import Expect exposing (Expectation)
import Parser.Advanced as P
import Stage.Parse.Parser
import Test exposing (Test, describe, test)


moduleDeclaration : Test
moduleDeclaration =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.moduleDeclaration
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleDeclaration"
        [ describe "general"
            (List.map runTest
                [ ( "works with simple module name"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "works with nested module name"
                  , "module Foo.Bar exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar", ExposingAll )
                  )
                , ( "works with even more nested module name"
                  , "module Foo.Bar.Baz.Quux exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo.Bar.Baz.Quux", ExposingAll )
                  )
                , ( "allows multiple spaces between the `module` keyword and the module name"
                  , "module  Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the module name and the `exposing` keyword"
                  , "module Foo  exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the module name and the `exposing` keyword"
                  , "module Foo\nexposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows multiple spaces between the `exposing` keyword and the exposing list"
                  , "module Foo exposing  (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "allows a newline between the `exposing` keyword and the exposing list"
                  , "module Foo exposing\n(..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "module Foo exposing"
                  , Nothing
                  )
                ]
            )
        , describe "plain module"
            (List.map runTest
                [ ( "simply works"
                  , "module Foo exposing (..)"
                  , Just ( PlainModule, ModuleName "Foo", ExposingAll )
                  )
                ]
            )
        , describe "port module"
            (List.map runTest
                [ ( "simply works"
                  , "port module Foo exposing (..)"
                  , Just ( PortModule, ModuleName "Foo", ExposingAll )
                  )
                ]
            )
        ]


exposingList : Test
exposingList =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.exposingList
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.exposingList"
        [ describe "exposing all"
            (List.map runTest
                [ ( "simply works"
                  , "(..)"
                  , Just ExposingAll
                  )
                , ( "doesn't work with spaces inside the parens"
                  , "( .. )"
                  , Nothing
                  )
                ]
            )
        , describe "exposing some"
            [ describe "general"
                (List.map runTest
                    [ ( "can't be empty"
                      , "()"
                      , Nothing
                      )
                    , ( "works with spaces between items"
                      , "(foo, bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with even more spaces between items"
                      , "(foo  ,  bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    , ( "works with mixed values"
                      , "(foo, Bar, Baz(..))"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedType "Bar"
                                , ExposedTypeAndAllConstructors "Baz"
                                ]
                            )
                      )
                    , ( "allows for newline"
                      , "(foo\n,bar)"
                      , Just
                            (ExposingSome
                                [ ExposedValue "foo"
                                , ExposedValue "bar"
                                ]
                            )
                      )
                    ]
                )
            , describe "values"
                (List.map runTest
                    [ ( "works with a value"
                      , "(foo)"
                      , Just (ExposingSome [ ExposedValue "foo" ])
                      )
                    ]
                )
            , describe "types"
                (List.map runTest
                    [ ( "works with exposed type"
                      , "(Foo)"
                      , Just (ExposingSome [ ExposedType "Foo" ])
                      )
                    ]
                )
            , describe "types with all constructors"
                (List.map runTest
                    [ ( "works with exposed type and all constructors"
                      , "(Foo(..))"
                      , Just (ExposingSome [ ExposedTypeAndAllConstructors "Foo" ])
                      )
                    , ( "doesn't allow spaces between the module name and the double period list"
                      , "(Foo (..))"
                      , Nothing
                      )
                    , ( "doesn't allow spaces inside the double period list"
                      , "(Foo( .. ))"
                      , Nothing
                      )
                    , ( "doesn't allow only some constructors exposed"
                      , "(Foo(Bar))"
                      , Nothing
                      )
                    ]
                )
            ]
        ]


dependencies : Test
dependencies =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.dependencies
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.dependencies"
        [ describe "general"
            (List.map runTest
                [ ( "allows for multiple modifiers"
                  , "import Foo as F exposing (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple spaces"
                  , "import   Foo   as   F   exposing   (..)"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Just ExposingAll
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple imports"
                  , "import Foo\nimport Bar"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "allows for multiple newlines between imports"
                  , "import Foo\n\nimport Bar"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            , ( ModuleName "Bar"
                              , { moduleName = ModuleName "Bar"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't allow for lower-case import"
                  , "import foo"
                  , Nothing
                  )
                ]
            )
        , describe "simple"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                ]
            )
        , describe "as"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo as F"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Just (ModuleName "F")
                                , exposing_ = Nothing
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work with lowercase alias"
                  , "import Foo as f"
                  , Nothing
                  )
                , ( "doesn't work with dot-separated alias"
                  , "import Foo as X.B"
                  , Nothing
                  )
                ]
            )
        , describe "exposing"
            (List.map runTest
                [ ( "simply works"
                  , "import Foo exposing (bar, Baz, Quux(..))"
                  , Just
                        (Dict.Any.fromList Common.moduleNameToString
                            [ ( ModuleName "Foo"
                              , { moduleName = ModuleName "Foo"
                                , as_ = Nothing
                                , exposing_ =
                                    Just
                                        (ExposingSome
                                            [ ExposedValue "bar"
                                            , ExposedType "Baz"
                                            , ExposedTypeAndAllConstructors "Quux"
                                            ]
                                        )
                                }
                              )
                            ]
                        )
                  )
                , ( "doesn't work without something after the `exposing` keyword"
                  , "import Foo exposing"
                  , Nothing
                  )
                ]
            )
        ]


moduleName : Test
moduleName =
    let
        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.moduleName
                        |> Result.toMaybe
                        |> Expect.equal output
    in
    describe "Stage.Parse.Parser.moduleName"
        (List.map runTest
            [ ( "works with simple module name"
              , "Foo"
              , Just "Foo"
              )
            , ( "doesn't work with lower-case name"
              , "foo"
              , Nothing
              )
            , ( "doesn't work with dot at the end"
              , "Foo."
              , Nothing
              )
            , ( "works with dotted module name"
              , "Foo.Bar"
              , Just "Foo.Bar"
              )
            , ( "works with doubly-dotted module name"
              , "Foo.Bar.Baz"
              , Just "Foo.Bar.Baz"
              )
            , ( "doesn't work with lower-case letter after the dot"
              , "Foo.bar"
              , Nothing
              )
            ]
        )


singleQuote : String -> String
singleQuote txt =
    "'" ++ txt ++ "'"


doubleQuote : String -> String
doubleQuote txt =
    "\"" ++ txt ++ "\""


tripleQuote : String -> String
tripleQuote txt =
    "\"\"\"" ++ txt ++ "\"\"\""


expr : Test
expr =
    let
        runSection ( description, tests ) =
            describe description
                (List.map runTest tests)

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.expr
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.expr"
        (List.map runSection
            [ ( "lambda"
              , [ ( "works with single argument"
                  , "\\x -> x + 1"
                  , Ok
                    (located (1, 1) 0
                        (AST.Frontend.lambda
                            [ VarName "x" ]
                            (located (1, 7) 6 (Plus
                                (located (1, 7) 6 (Argument (VarName "x")) 7)
                                (located (1, 11) 10 (Literal (Int 1)) 11)
                            ) 11)
                        )
                        11
                    )
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  , Ok (Located ({ end = 13, position = (1,1), start = 0 },Lambda { arguments = [VarName "x",VarName "y"], body = Located ({ end = 13, position = (1,9), start = 8 },Plus (Located ({ end = 9, position = (1,9), start = 8 },Argument (VarName "x"))) (Located ({ end = 13, position = (1,13), start = 12 },Argument (VarName "y")))) }))
                  )
                ]
              )
            , ( "call"
              , [ ( "simple"
                  , "fn 1"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Call { argument = Located ({ end = 4, position = (1,4), start = 3 },Literal (Int 1)), fn = Located ({ end = 2, position = (1,1), start = 0 },Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                , ( "with var"
                  , "fn arg"
                  , Ok (Located ({ end = 6, position = (1,1), start = 0 },Call { argument = Located ({ end = 6, position = (1,4), start = 3 },Var { name = VarName "arg", qualifier = Nothing }), fn = Located ({ end = 2, position = (1,1), start = 0 },Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Ok (Located ({ end = 12, position = (1,1), start = 0 },Call { argument = Located ({ end = 12, position = (1,9), start = 8 },Var { name = VarName "arg2", qualifier = Nothing }), fn = Located ({ end = 7, position = (1,1), start = 0 },Call { argument = Located ({ end = 7, position = (1,4), start = 3 },Var { name = VarName "arg1", qualifier = Nothing }), fn = Located ({ end = 2, position = (1,1), start = 0 },Var { name = VarName "fn", qualifier = Nothing }) }) }))
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Ok (Located ({ end = 7, position = (1,1), start = 0 },Call { argument = Located ({ end = 7, position = (1,4), start = 3 },Var { name = VarName "arg1", qualifier = Nothing }), fn = Located ({ end = 2, position = (1,1), start = 0 },Var { name = VarName "fn", qualifier = Nothing }) }))
                  )
                ]
              )
            , ( "if"
              , [ ( "with one space"
                  , "if 1 then 2 else 3"
                  , Ok (Located ({ end = 18, position = (1,1), start = 0 },If { else_ = Located ({ end = 18, position = (1,18), start = 17 },Literal (Int 3)), test = Located ({ end = 4, position = (1,4), start = 3 },Literal (Int 1)), then_ = Located ({ end = 11, position = (1,11), start = 10 },Literal (Int 2)) }))
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  , Ok (Located ({ end = 28, position = (1,1), start = 0 },If { else_ = Located ({ end = 28, position = (1,28), start = 27 },Literal (Int 3)), test = Located ({ end = 6, position = (1,6), start = 5 },Literal (Int 1)), then_ = Located ({ end = 17, position = (1,17), start = 16 },Literal (Int 2)) }))
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Int 123)))
                  )
                , ( "zero"
                  , "0"
                  , Ok (Located ({ end = 1, position = (1,1), start = 0 },Literal (Int 0)))
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  , Ok (Located ({ end = 8, position = (1,1), start = 0 },Literal (Int 1194684)))
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Ok (Located ({ end = 8, position = (1,1), start = 0 },Literal (Int 7904751)))
                  )
                , ( "negative int"
                  , "-42"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Int -42)))
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Ok (Located ({ end = 9, position = (1,1), start = 0 },Literal (Int -1194684)))
                  )
                ]
              )
            , ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Float 12.3)))
                  )
                , ( "zero"
                  , "0.0"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Float 0)))
                  )
                , ( "negative float"
                  , "-4.2"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Float -4.2)))
                  )
                , ( "Scientific notation"
                  , "5.12e2"
                  , Ok (Located ({ end = 6, position = (1,1), start = 0 },Literal (Float 512)))
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  , Ok (Located ({ end = 6, position = (1,1), start = 0 },Literal (Float 512)))
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  , Ok (Located ({ end = 7, position = (1,1), start = 0 },Literal (Float -512)))
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Float 0.05)))
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Char '1')))
                  )
                , ( "space"
                  , "' '"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Char (' '))))
                  )
                , ( "letter lowercase"
                  , "'a'"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Char 'a')))
                  )
                , ( "letter uppercase"
                  , "'A'"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Char 'A')))
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape n"
                  , singleQuote "\\n"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Char '\n')))
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Char '\r')))
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Char '\t')))
                  )
                , ( "double quote"
                  , singleQuote "\""
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (Char '"')))
                    -- " (for vscode-elm bug)
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Char '\'')))
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Char '😃')))
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  , Ok (Located ({ end = 11, position = (1,1), start = 0 },Literal (Char '🙈')))
                  )
                ]
              )
            , ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  , Ok (Located ({ end = 2, position = (1,1), start = 0 },Literal (String "")))
                  )
                , ( "one space"
                  , doubleQuote " "
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (String " ")))
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "42")))
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },Literal (String "'")))
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "\"")))
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "\\n")))
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "\\r")))
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "\\t")))
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (String "😃")))
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  , Ok (Located ({ end = 11, position = (1,1), start = 0 },Literal (String "\\u{1F648}")))
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\abc123"
                  , Ok (Located ({ end = 24, position = (1,1), start = 0 },Literal (String "\\u{1F648}\\n\\r\\t\\abc123")))
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  , Ok (located (0,0) 0 (Literal (String "")) 6)
                  )
                , ( "one space"
                  , tripleQuote " "
                  , Ok (located (0,0) 0 (Literal (String " ")) 7)
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  , Ok (located (0,0) 0 (Literal (String "\n")) 8)
                  )
                , ( "two numbers"
                  , tripleQuote "42"
                  , Ok (located (0,0) 0 (Literal (String "42")) 8)
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  , Ok (located (0,0) 0 (Literal (String "'")) 7)
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  , Ok (located (0,0) 0 (Literal (String " \" ")) 9)
                  )
                , ( "escape n"
                  , tripleQuote "\\n"
                  , Ok (located (0,0) 0 (Literal (String "\n")) 8)
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  , Ok (located (0,0) 0 (Literal (String "\u{000D}")) 18)
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  , Ok (located (0,0) 0 (Literal (String "\t")) 8)
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  , Ok (located (0,0) 0 (Literal (String "😃")) 8)
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  , Ok (located (0,0) 0 (Literal (String "🙈")) 8)
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\abc123"
                  , Ok (located (0,0) 0 (Literal (String "🙈\n\n\n\u{000D}\t\\abc123")) 1)
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },Literal (Bool True)))
                  )
                , ( "False"
                  , "False"
                  , Ok (Located ({ end = 5, position = (1,1), start = 0 },Literal (Bool False)))
                  )
                ]
              )
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  , Ok (Located ({ end = 14, position = (1,1), start = 0 },Let { bindings = [{ body = Located ({ end = 9, position = (1,9), start = 8 },Literal (Int 1)), name = VarName "x" }], body = Located ({ end = 14, position = (1,14), start = 13 },Literal (Int 2)) }))
                  )
                , ( "one binding, generous whitespace"
                  , "let\n  x =\n      1\nin\n  2"
                  , Ok (Located ({ end = 24, position = (1,1), start = 0 },Let { bindings = [{ body = Located ({ end = 17, position = (3,7), start = 16 },Literal (Int 1)), name = VarName "x" }], body = Located ({ end = 24, position = (5,3), start = 23 },Literal (Int 2)) }))
                  )
                ]
              )
            , ( "list"
              , [ ( "empty list"
                  , "[]"
                  , Ok (Located ({ end = 2, position = (1,1), start = 0 },List []))
                  )
                , ( "empty list with inner spaces"
                  , "[  ]"
                  , Ok (Located ({ end = 4, position = (1,1), start = 0 },List []))
                  )
                , ( "single item in list"
                  , "[1]"
                  , Ok (Located ({ end = 3, position = (1,1), start = 0 },List [Located ({ end = 2, position = (1,2), start = 1 },Literal (Int 1))]))
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  , Ok (Located ({ end = 5, position = (1,1), start = 0 },List [Located ({ end = 3, position = (1,3), start = 2 },Literal (Int 1))]))
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  , Ok (Located ({ end = 7, position = (1,1), start = 0 },List [Located ({ end = 2, position = (1,2), start = 1 },Literal (Int 1)),Located ({ end = 4, position = (1,4), start = 3 },Literal (Int 2)),Located ({ end = 6, position = (1,6), start = 5 },Literal (Int 3))]))
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  , Ok (Located ({ end = 14, position = (1,1), start = 0 },List [Located ({ end = 3, position = (1,3), start = 2 },Literal (Int 1)),Located ({ end = 7, position = (1,7), start = 6 },Literal (Int 2)),Located ({ end = 12, position = (1,12), start = 11 },Literal (Int 3))]))
                  )
                ]
              )
            , ( "unit"
              , [ ( "simple case"
                  , "()"
                  , Ok (Located ({ end = 2, position = (1,1), start = 0 },Unit))
                  )
                ]
              )
            ]
        )


expectEqualParseResult :
    String
    -> Result (List (P.DeadEnd ParseContext ParseProblem)) a
    -> Result (List (P.DeadEnd ParseContext ParseProblem)) a
    -> Expectation
expectEqualParseResult input expected actual =
    if actual == expected then
        Expect.pass

    else
        case actual of
            Err deadEnds ->
                Expect.fail
                    (String.join "\n"
                        (input
                            :: "===>"
                            :: "Err"
                            :: List.map deadEndToString deadEnds
                        )
                    )

            _ ->
                actual |> Expect.equal expected


deadEndToString : P.DeadEnd ParseContext ParseProblem -> String
deadEndToString deadEnd =
    let
        metadata =
            "("
                ++ String.fromInt (deadEnd.row - 1)
                ++ ","
                ++ String.fromInt (deadEnd.col - 1)
                ++ ") "
                ++ Debug.toString deadEnd.problem
    in
    String.join "\n    "
        ("\n"
            :: metadata
            :: "---- with context stack ----"
            :: List.map contextToString deadEnd.contextStack
        )


contextToString : { row : Int, col : Int, context : ParseContext } -> String
contextToString context =
    "("
        ++ String.fromInt (context.row - 1)
        ++ ","
        ++ String.fromInt (context.col - 1)
        ++ ") "
        ++ Debug.toString context.context


