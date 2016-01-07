module ExprParser (toExpr) where

import String

import Interpreter exposing (Expr (..), Atom (..), Prim (..), Name)
import SExprParser as SExpr exposing (SExpr)
import Utils


toArgs : SExpr -> Maybe (List Name)
toArgs sexpr =
  case sexpr of
    SExpr.ListExpr subExprs ->
        subExprs
          |> List.map (\subExpr () ->
            case subExpr of
                SExpr.SymExpr name -> Just name
                _ -> Nothing)
          |> Utils.requireAll
    _ -> Nothing


handleAll : List SExpr -> Maybe (List Expr)
handleAll sexprs =
    sexprs
      |> List.map (\sexpr () -> toExpr sexpr)
      |> Utils.requireAll


handleHead : String -> List SExpr -> Maybe Expr
handleHead head rest =
  case head of
    
    "fn" ->
      case rest of
        [args, body] ->
            Maybe.map2 AnonFunExpr (toArgs args) (toExpr body)
        _ -> Nothing

    "if" ->
      case rest of
        [condition, thenClause, elseClause] ->
            Maybe.map3 IfExpr
              (toExpr condition)
              (toExpr thenClause)
              (toExpr elseClause)
        _ -> Nothing

    _ ->
        -- TODO: This parses the arguments even if parsing the callee failed,
        -- which is slightly inefficient.
        Maybe.map2 AppExpr (handleSym head) (handleAll rest)


isUpper : Char -> Bool
isUpper c =
    let str = String.fromChar c
    in not (str == String.toLower str)


handleSym : String -> Maybe Expr
handleSym sym =
  case sym of
    "+" -> Just <| LitExpr <| PrimAtom AddPrim
    "-" -> Just <| LitExpr <| PrimAtom SubPrim
    "*" -> Just <| LitExpr <| PrimAtom MulPrim
    "/" -> Just <| LitExpr <| PrimAtom DivPrim

    "=" -> Just <| LitExpr <| PrimAtom EqualPrim
    "<" -> Just <| LitExpr <| PrimAtom LessPrim
    "<=" -> Just <| LitExpr <| PrimAtom LessEqualPrim

    "type-of" -> Just <| LitExpr <| PrimAtom TypeOfPrim

    _ ->
      case String.uncons sym of
        -- TODO: Is there a more graceful way to handle this?
        -- Should this just result in a parse error?
        -- Failing with a normal parse error seems like undue error-silencing.
        Nothing -> Debug.crash "Invariant broken: the tokenizer should never generate empty s-expression symbols"

        Just (firstChar, _) ->
          if isUpper firstChar
            then Just (LitExpr (SymAtom sym))
            else Just (NamedExpr sym)


toExpr : SExpr -> Maybe Expr
toExpr sexpr =
  case sexpr of
    
    SExpr.NumExpr num ->
        Just (LitExpr (NumAtom num))

    SExpr.SymExpr sym ->
        handleSym sym

    SExpr.ListExpr ((SExpr.SymExpr head) :: rest) ->
        handleHead head rest

    SExpr.ListExpr (callee :: args) ->
        Maybe.map2 AppExpr (toExpr callee) (handleAll args)

    _ -> Nothing
