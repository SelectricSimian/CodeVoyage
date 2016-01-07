-- STRICT VERSION

module Interpreter where

--import Lazy exposing (Lazy, lazy, force)
import Dict exposing (Dict)
import Set exposing (Set)

{-
    Abbreviations
    (As a style convention, these abbreviations should *always* be used, and should there full versions always avoided)

    Num  <-> Number
    Sym  <-> Symbol
    Anon <-> Anonymous
    Fun  <-> Function
    Prim <-> Primitive
    Val  <-> Value
    App  <-> Application / Apply
    Arg  <-> Argument
    Expr <-> Expression

    Sub <-> Subtract
    Mul <-> Multiply
    Div <-> Divide
-}

type alias Name = String


type alias Num = Float


type alias Sym = String


type Prim

    -- Arithmetic
    = AddPrim
    | SubPrim
    | MulPrim
    | DivPrim

    -- Comparison
    | EqualPrim
    | LessPrim
    | LessEqualPrim

    -- Utilities
    | TypeOfPrim


-- function name field should be non-Nothing iff it is a global
type alias Fun =
    { globalName : Maybe String
    , args : List Name
    , body : Expr 
    }


type Atom
    = NumAtom Num
    | SymAtom Sym
    | PrimAtom Prim
    | ErrorAtom


type Val
    = AtomVal Atom
    | FunVal Fun
    | StructVal Sym (List TrackedVal)


type History

    = AppHistory
        { fun : TrackedVal
        , args : List TrackedVal
        , internalHistory : History
        }

    | PrimHistory
        { prim : TrackedVal
        , args : List TrackedVal
        }

    | LitHistory

    | IfHistory
        { condition : TrackedVal
        , thenClause : Expr
        , elseClause : Expr
        , internalHistory : History
        }

-- TODO: Add ErrorHistory variant or something similar, which holds an
-- unevaluated source expression which would evaluate to an error.
-- Currently, many errors are created with LitHistory as their history,
-- which is unconveniently nonspecific for the user.


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


type alias Globals = Dict Name TrackedVal


type alias Bindings = Dict Name TrackedVal


eval : Globals -> Expr -> TrackedVal
eval globals expr =
  case expr of

    LitExpr atom ->
        { result = AtomVal atom, history = LitHistory }

    NamedExpr name ->
        -- note: argument substitution is done separately, and we therefore do
        -- not attempt to resolve argument names here
        Dict.get name globals
            |> Maybe.withDefault
                { result = AtomVal ErrorAtom
                , history = LitHistory
                }
            -- TODO: Replace LitHistory with something more informative

    AnonFunExpr args body ->
        { result = FunVal (Fun Nothing args body), history = LitHistory }

    AppExpr callee args ->
      let
        calleeVal = eval globals callee
        argVals = List.map (eval globals) args
      in case calleeVal.result of
        AtomVal (PrimAtom prim) -> applyPrim (calleeVal.history, prim) argVals
        FunVal fun -> applyFun globals (calleeVal.history, fun) argVals
        AtomVal (SymAtom sym) ->
            { result = StructVal sym argVals
            -- TODO: should this really be LitHistory?
            , history = LitHistory
            }
        _ ->
            { result = AtomVal ErrorAtom
            -- TODO: this should DEFINITELY not be LitHistory 
            , history = LitHistory
            }


    IfExpr condition thenClause elseClause ->
      let
        conditionVal = eval globals condition

        result = case conditionVal.result of
            AtomVal (SymAtom "True") -> eval globals thenClause
            AtomVal (SymAtom "False") -> eval globals elseClause

            -- TODO: this should be changed from LitHistory
            _ -> { history = LitHistory, result = AtomVal ErrorAtom }

        history = IfHistory
            { condition = conditionVal
            , thenClause = thenClause
            , elseClause = elseClause
            , internalHistory = result.history
            }
      in
        { result = result.result, history = history }

    SubstitutedExpr val -> val


substitute : Bindings -> Expr -> Expr
substitute substitutions expr =
  let recurse = substitute substitutions
  in case expr of

    LitExpr _ -> expr

    NamedExpr name ->
        Dict.get name substitutions
          |> Maybe.map SubstitutedExpr
          |> Maybe.withDefault (NamedExpr name)

    AnonFunExpr args body ->
        AnonFunExpr args (recurse body)

    AppExpr callee args ->
        AppExpr (recurse callee) (List.map (recurse) args)

    IfExpr condition thenClause elseClause ->
        IfExpr (recurse condition) (recurse thenClause) (recurse elseClause)

    SubstitutedExpr _ -> expr


lazyAndHandler : (() -> Maybe Bool) -> Bool -> Maybe Bool
lazyAndHandler handler precondition =
  if precondition
    then handler ()
    else Just False


listEqual : (a -> a -> Maybe Bool) -> List a -> List a -> Maybe Bool
listEqual equal lst1 lst2 =
  case (lst1, lst2) of
    ([],  []) -> Just True
    ([], _) -> Just False
    (_, []) -> Just False

    (x :: xs, y :: ys) ->
        Maybe.andThen
            (equal x y)
            (lazyAndHandler (\() -> listEqual equal xs ys))


valEqual : Val -> Val -> Maybe Bool
valEqual val1 val2 =
  case (val1, val2) of
    -- illegal types to compare
    
    (FunVal _, _) -> Nothing
    (_, FunVal _) -> Nothing

    (AtomVal (PrimAtom _), _) -> Nothing
    (_, AtomVal (PrimAtom _)) -> Nothing

    (AtomVal ErrorAtom, _) -> Nothing
    (_, AtomVal ErrorAtom) -> Nothing

    -- legal types to compare

    (AtomVal (NumAtom a), AtomVal (NumAtom b)) -> Just (a == b)
    (AtomVal (SymAtom a), AtomVal (SymAtom b)) -> Just (a == b)

    (StructVal head1 args1, StructVal head2 args2) ->
      let
        arg1Results = List.map .result args1
        arg2Results = List.map .result args2
      in
        listEqual valEqual arg1Results arg2Results
        |> Maybe.map ((&&) (head1 == head2))

    -- we've already eliminated every illegal possibility, and every legal
    -- possibility where the values could possibly be equal, so they must be
    -- unequal
    _ -> Just False


boolToSym : Bool -> Val
boolToSym bool =
  if bool
    then AtomVal (SymAtom "True")
    else AtomVal (SymAtom "False")


typeOf : Val -> Maybe String
typeOf val =
  case val of
    AtomVal (NumAtom _) -> Just "Num"
    AtomVal (SymAtom _) -> Just "Sym"
    AtomVal (PrimAtom _) -> Just "Prim"
    AtomVal ErrorAtom -> Nothing
    FunVal _ -> Just "Fun"
    StructVal _ _ -> Just "Struct"


--applyPrim : (History, Prim) -> List TrackedVal -> TrackedVal
--applyPrim (primHistory, prim) args =
--  let
--    history = PrimHistory
--        { prim = { history = primHistory, result = AtomVal (PrimAtom prim) }
--        , args = args
--        }

--    argResults = List.map .result args

--    result =
--      case (prim, argResults) of
        
--        -- Arithmetic

--        (AddPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            AtomVal (NumAtom (a + b))

--        (SubPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            AtomVal (NumAtom (a - b))

--        (MulPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            AtomVal (NumAtom (a * b))

--        (DivPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            AtomVal (NumAtom (a / b))

--        -- Comparison

--        (EqualPrim, [val1, val2]) ->
--            valEqual val1 val2
--              |> Maybe.map boolToSym
--              |> Maybe.withDefault (AtomVal ErrorAtom)

--        (LessPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            boolToSym (a < b)

--        -- implementing ordering for symbols is questionable
--        (LessPrim, [AtomVal (SymAtom a), AtomVal (SymAtom b)]) ->
--            boolToSym (a < b)

--        (LessEqualPrim, [AtomVal (NumAtom a), AtomVal (NumAtom b)]) ->
--            boolToSym (a <= b)

--        -- once again, implementing ordering for symbols is questionable
--        (LessEqualPrim, [AtomVal (SymAtom a), AtomVal (SymAtom b)]) ->
--            boolToSym (a <= b)

--        -- Utilities

--        (TypeOfPrim, [val]) ->
--            typeOf val
--              |> Maybe.map (AtomVal << SymAtom)
--              |> Maybe.withDefault (AtomVal ErrorAtom)

--        _ -> AtomVal ErrorAtom
--  in
--    { history = history, result = result }

-- This could be written in a more concise way in which every erroneous case is
-- handled by a single wildcard pattern at the very end of a single case
-- expression, but for some reason the Elm compiler takes a VERY long time to
-- verify the comprehensiveness of such a case expression.  For that reason,
-- it has been restructured to have an additional level of nested case
-- statements, such that both levels of case statements are themselves very
-- simple and easily checked by the compiler.
applyPrim : (History, Prim) -> List TrackedVal -> TrackedVal
applyPrim (primHistory, prim) args =
  let
    history = PrimHistory
        { prim = { history = primHistory, result = AtomVal (PrimAtom prim) }
        , args = args
        }

    argResults = List.map .result args

    result =
      case prim of
        
        -- Arithmetic

        AddPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                AtomVal (NumAtom (a + b))
            _ -> AtomVal ErrorAtom

        SubPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                AtomVal (NumAtom (a - b))
            _ -> AtomVal ErrorAtom

        MulPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                AtomVal (NumAtom (a * b))
            _ -> AtomVal ErrorAtom

        DivPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                AtomVal (NumAtom (a / b))
            _ -> AtomVal ErrorAtom

        -- Comparison

        EqualPrim ->
          case argResults of
            [val1, val2] ->
                valEqual val1 val2
                  |> Maybe.map boolToSym
                  |> Maybe.withDefault (AtomVal ErrorAtom)
            _ -> AtomVal ErrorAtom

        LessPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                boolToSym (a < b)
            -- implementing ordering for symbols is questionable
            [AtomVal (SymAtom a), AtomVal (SymAtom b)] ->
                boolToSym (a < b)
            _ -> AtomVal ErrorAtom

        LessEqualPrim ->
          case argResults of
            [AtomVal (NumAtom a), AtomVal (NumAtom b)] ->
                boolToSym (a <= b)
            -- implementing ordering for symbols is questionable
            [AtomVal (SymAtom a), AtomVal (SymAtom b)] ->
                boolToSym (a <= b)
            _ -> AtomVal ErrorAtom

        -- Utilities

        TypeOfPrim ->
          case argResults of
            [val] ->
                typeOf val
                  |> Maybe.map (AtomVal << SymAtom)
                  |> Maybe.withDefault (AtomVal ErrorAtom)
            _ -> AtomVal ErrorAtom
  in
    { history = history, result = result }


maybeZip : List a -> List b -> Maybe (List (a, b))
maybeZip lst1 lst2 =
  case (lst1, lst2) of
    ([], []) -> Just []
    (x :: xs, y :: ys) -> Maybe.map ((::) (x, y)) <| maybeZip xs ys
    _ -> Nothing


applyFun : Globals -> (History, Fun) -> List TrackedVal -> TrackedVal
applyFun globals (funHistory, fun) args =
  case maybeZip fun.args args of

    -- this should *definitely* not be LitHistory
    Nothing -> { history = LitHistory, result = AtomVal ErrorAtom }

    Just namedArgs ->
      let
        substitutions = Dict.fromList namedArgs
        substitutedBody = substitute substitutions fun.body
        internal = eval globals substitutedBody
        history = AppHistory
            { fun = { history = funHistory, result = FunVal fun }
            , args = args
            , internalHistory = internal.history
            }
      in
        { history = history, result = internal.result }
