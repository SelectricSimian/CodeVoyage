module Illuminator where

import String

import Html exposing (Html, text, span)
import List exposing (concat)
import Html.Events as Events
import Html.Attributes as Attr

import Interpreter exposing (Name, Sym, Atom (..), Prim (..))

type AbstractionExpansion = Unexpanded | ShallowExpanded | DeepExpanded


-- TODO: Consider reducing coupling between Illuminator and Interpreter by
-- including type parameter for recursion or annotation in interpreter data
-- types.  Right now there's a lot of redundant type definition.

-- That being said, this module requires intimate knowledge of the structure
-- and variants of tracked values and expressions anyway, because it has to
-- display them, so maybe it's some degree of tight coupling is inevitable.

type History
    
    = AppHistory
        { fun : TrackedVal
        , args : List TrackedVal
        -- TODO: make it impossible for internalHistory to be in a completely
        -- unexpanded state
        , internalHistory : History
        , expansion : AbstractionExpansion
        }

    | PrimHistory
        { prim : TrackedVal
        , args : List TrackedVal
        , expanded : Bool
        }

    | LitHistory

    | IfHistory
        { condition : TrackedVal
        , thenClause : Expr
        , elseClause : Expr
        -- TODO: make it impossible for internalHistory to be in a completely
        -- unexpanded state
        , internalHistory : History
        , expansion : AbstractionExpansion
        }


type Val
    = AtomVal Atom
    | FunVal Fun
    | StructVal Sym (List TrackedVal)


type alias Fun =
    { globalName : Maybe String
    , args : List Name
    , body : Expr
    }


type alias TrackedVal =
    { result : Val
    , history : History
    }


type Expr
    = LitExpr Atom
    | NamedExpr Name
    | SubstitutedExpr TrackedVal
    | AnonFunExpr (List Name) Expr
    | AppExpr Expr (List Expr)
    | IfExpr Expr Expr Expr


annotateTracked : Interpreter.TrackedVal -> TrackedVal
annotateTracked val =
    { history = annotateHistory val.history, result = annotateVal val.result }

annotateVal : Interpreter.Val -> Val
annotateVal val =
  case val of
    
    Interpreter.AtomVal atom ->
        AtomVal atom
    
    Interpreter.FunVal fun ->
      FunVal
        { globalName = fun.globalName
        , args = fun.args
        , body = annotateExpr fun.body
        }

    Interpreter.StructVal head components ->
        StructVal head (List.map annotateTracked components)


annotateExpr : Interpreter.Expr -> Expr
annotateExpr expr =
  case expr of
    
    Interpreter.LitExpr atom ->
        LitExpr atom

    Interpreter.NamedExpr name ->
        NamedExpr name

    Interpreter.SubstitutedExpr val ->
        SubstitutedExpr (annotateTracked val)

    Interpreter.AnonFunExpr args body ->
        AnonFunExpr args (annotateExpr body)

    Interpreter.AppExpr callee args ->
        AppExpr (annotateExpr callee) (List.map annotateExpr args)

    Interpreter.IfExpr condition thenClause elseClause ->
        IfExpr
            (annotateExpr condition)
            (annotateExpr thenClause)
            (annotateExpr elseClause)


annotateHistory : Interpreter.History -> History
annotateHistory history =
  case history of
    
    Interpreter.AppHistory appHistory ->
      AppHistory
        { fun = annotateTracked appHistory.fun
        , args = List.map annotateTracked appHistory.args
        , internalHistory = annotateHistory appHistory.internalHistory
        , expansion = Unexpanded
        }

    Interpreter.PrimHistory primHistory ->
      PrimHistory
        { prim = annotateTracked primHistory.prim
        , args = List.map annotateTracked primHistory.args
        , expanded = False
        }

    Interpreter.LitHistory -> LitHistory

    Interpreter.IfHistory ifHistory ->
      IfHistory
        { condition = annotateTracked ifHistory.condition
        , thenClause = annotateExpr ifHistory.thenClause
        , elseClause = annotateExpr ifHistory.elseClause
        , internalHistory = annotateHistory ifHistory.internalHistory
        , expansion = Unexpanded
        }


primName : Prim -> String
primName prim =
  case prim of
    AddPrim -> "+"
    SubPrim -> "-"
    MulPrim -> "*"
    DivPrim -> "/"

    EqualPrim -> "="
    LessPrim -> "<"
    LessEqualPrim -> "<="

    TypeOfPrim -> "type-of"


formatAtom : Atom -> String
formatAtom atom =
  case atom of
    NumAtom num -> toString num
    SymAtom sym -> toString sym
    ErrorAtom -> "error"
    PrimAtom prim -> primName prim

viewList
    :  (Signal.Address a -> a -> Html)
    -> Signal.Address (List a)
    -> List a
    -> List Html
viewList viewItem address lst =
  case lst of
    [] -> []
    (x :: xs) ->
        (viewItem (Signal.forwardTo address (flip (::) xs)) x)
          :: viewList viewItem (Signal.forwardTo address ((::) x)) xs


type alias AnonFun = { args : List Name, body : Expr }


viewAnonFun : Signal.Address AnonFun -> AnonFun -> Html
viewAnonFun address fun =
  span []
    [ text ("(fn (" ++ (String.join " " fun.args) ++ ") ")
    , viewExpr 
        (Signal.forwardTo
            address
            (\newBody -> { fun | body = newBody }))
        fun.body
    , text ")"
    ]

viewVal : Signal.Address Val -> Val -> Html
viewVal address val =
  case val of

    AtomVal atom ->
        text (formatAtom atom)

    FunVal fun ->
        case fun.globalName of
            Just name -> text name
            Nothing ->
              viewAnonFun
                (Signal.forwardTo
                    address
                    (\anonFun -> FunVal
                        { globalName = Nothing
                        , args = anonFun.args
                        , body = anonFun.body }))
                { args = fun.args, body = fun.body }

    StructVal head components ->
      span [] (concat 
        [ [ text ("(" ++ head ++ " ")]
        , components
            |> viewList viewTracked
              (Signal.forwardTo
                address
                (\newComponents -> StructVal head newComponents))
            |> List.concatMap (\viewed -> [viewed, text " "])
        , [ text ")" ]
        ])

viewExpr : Signal.Address Expr -> Expr -> Html
viewExpr address expr =
  case expr of

    LitExpr atom ->
        text (formatAtom atom)

    NamedExpr name ->
        text name

    SubstitutedExpr val ->
        viewTracked (Signal.forwardTo address SubstitutedExpr) val

    AnonFunExpr args body ->
      viewAnonFun
        (Signal.forwardTo
            address
            (\anonFun -> AnonFunExpr anonFun.args anonFun.body))
        { args = args, body = body }

    AppExpr callee args ->
      span [] (concat
        [ [ text "("
          , viewExpr
              (Signal.forwardTo address (\newCallee -> AppExpr newCallee args))
              callee
          , text " "
          ]
        , args
            |> viewList
                viewExpr
                (Signal.forwardTo address (\newArgs -> AppExpr callee newArgs))
            |> List.concatMap (\viewed -> [viewed, text " "])
        , [ text ")" ]
        ])

    IfExpr condition thenClause elseClause ->
      span []
        [ text "(if "
        , viewExpr
            (Signal.forwardTo
                address
                (\newCondition -> IfExpr newCondition thenClause elseClause))
            condition
        , text " "
        , viewExpr
            (Signal.forwardTo
                address
                (\newThenClause -> IfExpr condition newThenClause elseClause))
            thenClause
        , text " "
        , viewExpr
            (Signal.forwardTo
                address
                (\newElseClause -> IfExpr condition thenClause newElseClause))
            elseClause
        , text ")"
        ]


textButton : Signal.Address a -> a -> String -> Html
textButton address action content =
    Html.span
        [ Attr.style [ ("pointer", "default") ]
        , Events.onClick address action
        ]
        [ text content ]


viewTracked : Signal.Address TrackedVal -> TrackedVal -> Html
viewTracked address val =
  case val.history of

    AppHistory appHistory ->
      case appHistory.expansion of
        
        Unexpanded -> 
          span []
            [ textButton
                address
                {val | history =
                    AppHistory { appHistory | expansion = ShallowExpanded }}
                "[uneval]"
            , viewVal
                (Signal.forwardTo address (\new -> {val | result = new}))
                val.result
            ]

        ShallowExpanded ->
          span [] (concat
            [ [ textButton
                    address
                    {val | history =
                        AppHistory { appHistory | expansion = DeepExpanded }}
                    "[apply]"
              , textButton
                    address
                    {val | history =
                        AppHistory { appHistory | expansion = Unexpanded }}
                    "[eval]"
              , text "("
              , viewTracked
                    (Signal.forwardTo
                        address
                        (\newFun -> { val | history =
                            AppHistory { appHistory | fun = newFun } }))
                    appHistory.fun
              , text " "
              ]
            , appHistory.args
                |> viewList viewTracked
                    (Signal.forwardTo
                        address
                        (\newArgs -> { val | history =
                            AppHistory { appHistory | args = newArgs } }))
                |> List.concatMap (\viewed -> [viewed, text " "])
            , [ text ")" ]
            ])

        DeepExpanded ->
            span []
                [ textButton
                    address
                    {val | history =
                        AppHistory { appHistory | expansion = Unexpanded }}
                    "[eval]"
                , textButton
                    address
                    {val | history =
                      AppHistory
                        { appHistory | expansion = ShallowExpanded }}
                    "[unapply]"
                , viewTracked
                    -- TODO: Clean-up this expression
                    (Signal.forwardTo
                        address
                        (\new -> { val
                            | history = AppHistory { appHistory
                                | internalHistory = new.history
                                }
                            , result = new.result
                            }))
                    { history = appHistory.internalHistory
                    , result = val.result
                    }
                ]

    PrimHistory primHistory ->
      if primHistory.expanded
        then
          span [] (concat
            [ [ textButton
                    address
                    { val | history =
                        PrimHistory { primHistory | expanded = False } }
                    "[eval]"
              , text "("
              , viewTracked
                    (Signal.forwardTo
                        address
                        (\newPrim ->
                            { val | history =
                              PrimHistory
                                { primHistory | prim = newPrim } }))
                    primHistory.prim
              , text " "
              ]
            , primHistory.args
                |> viewList viewTracked
                    (Signal.forwardTo
                        address
                        (\newArgs ->
                            { val | history =
                              PrimHistory
                                { primHistory | args = newArgs } }))
                |> List.concatMap (\view -> [view, text " "])
            , [ text ")" ]
            ])
        else
          span []
            [ textButton
                address
                { val | history = PrimHistory { primHistory | expanded = True } }
                "[uneval]"
            , viewVal
                (Signal.forwardTo address (\new -> { val | result = new }))
                val.result
            ]

    IfHistory ifHistory ->
      case ifHistory.expansion of
        
        Unexpanded ->
          span []
            [ textButton
                address
                { val | history =
                    IfHistory { ifHistory | expansion = ShallowExpanded } }
                "[uneval]"
            , viewVal
                (Signal.forwardTo address (\new -> { val | result = new }))
                val.result
            ]

        ShallowExpanded ->
          span []
            [ textButton
                address
                { val | history =
                    IfHistory { ifHistory | expansion = Unexpanded } }
                "[eval]"
            , textButton
                address
                { val | history =
                    IfHistory { ifHistory | expansion = DeepExpanded } }
                "[branch]"
            , text "(if "
            , viewTracked
                (Signal.forwardTo
                    address
                    (\newCondition -> { val | history =
                        IfHistory { ifHistory | condition = newCondition } }))
                ifHistory.condition
            , text " "
            , viewExpr
                (Signal.forwardTo
                    address
                    (\newThenClause -> { val | history =
                      IfHistory
                        { ifHistory | thenClause = newThenClause } }))
                ifHistory.thenClause
            , text " "
            , viewExpr
                (Signal.forwardTo
                    address
                    (\newElseClause -> { val | history =
                      IfHistory
                        { ifHistory | elseClause = newElseClause } }))
                ifHistory.elseClause
            , text ")"
            ]

        DeepExpanded ->
          span []
            [ textButton
                address
                { val | history =
                    IfHistory { ifHistory | expansion = Unexpanded } }
                "[eval]"
            , textButton
                address
                { val | history =
                    IfHistory { ifHistory | expansion = ShallowExpanded } }
                "[unbranch]"
            , viewTracked
                (Signal.forwardTo
                    address
                    (\newVal ->
                      { val
                        | history =
                          IfHistory
                            { ifHistory | internalHistory = newVal.history }
                        , result = newVal.result
                        }))
                { result = val.result, history = ifHistory.internalHistory }
            ]

    LitHistory ->
        viewVal
            (Signal.forwardTo
                address
                (\new -> { val | result = new }))
            val.result
