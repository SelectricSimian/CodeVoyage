module SExprParser
    ( SExpr (..)
    , parse
    ) where

import String
import Char
import Utils

type SExpr
    = SymExpr String
    | NumExpr Float
    | ListExpr (List SExpr)


type Token
    = OpenToken
    | CloseToken
    | SymToken String
    | NumToken Float


-- TODO: Support unicode symbols
isAtomChar : Char -> Bool
isAtomChar c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c >= '0' && c <= '9') ||
    List.member c ['+', '-', '*', '/', '=', '<', '>', '%', '^', '_', '.']


takeWhile : (Char -> Bool) -> String -> (String, String)
takeWhile condition str =
 case String.uncons str of
    Nothing -> ("", "")
    Just (x, xs) ->
      if condition x
        then
            let (matched, unmatched) = takeWhile condition xs
            in (String.cons x matched, unmatched)
        else
            ("", str)


type alias Tokenizer = String -> Maybe (Token, String)


tokenizeParen : Tokenizer
tokenizeParen str =
  case String.uncons str of
    Just ('(', xs) -> Just (OpenToken, xs)
    Just (')', xs) -> Just (CloseToken, xs)
    _ -> Nothing


tokenizeAtom : Tokenizer
tokenizeAtom str =
    case takeWhile isAtomChar str of
        ("", _) -> Nothing
        (matched, unmatched) ->
            case String.toFloat matched of
                Ok float ->
                    -- Due to standard library bugs, some improperly formatted
                    -- input strings, such as ".", yield (Ok NaN) instead of
                    -- an (Err _), so we have to manually test for Nan
                    if isNaN float
                        then Just (SymToken matched, unmatched)
                        else Just (NumToken float, unmatched)
                _ -> Just (SymToken matched, unmatched)


tokenize1 : Tokenizer
tokenize1 str =
  Utils.tryAll
    [ \() -> tokenizeParen str
    , \() -> tokenizeAtom str
    ]

tokenize : String -> Maybe (List Token)
tokenize str =
    case String.trimLeft str of
        "" -> Just []
        trimmedStr ->
          Maybe.andThen
            (tokenize1 trimmedStr)
            (\(token, unmatched) ->
                Maybe.map ((::) token) <| tokenize unmatched)


partialParse1 : List Token -> Maybe (SExpr, List Token)
partialParse1 tokens =
    case tokens of

        (OpenToken :: xs) ->
          case partialParse xs of
            (subExprs, CloseToken :: xs') ->
                Just (ListExpr subExprs, xs')
            _ -> Nothing

        (SymToken sym :: xs) -> Just (SymExpr sym, xs)

        (NumToken num :: xs) -> Just (NumExpr num, xs)

        _ -> Nothing


partialParse : List Token -> (List SExpr, List Token)
partialParse =
    Utils.greedyIterate partialParse1

parseTokens : List Token -> Maybe (List SExpr)
parseTokens tokens =
  case partialParse tokens of
    (exprs, []) -> Just exprs
    _ -> Nothing

parse : String -> Maybe (List SExpr)
parse str =
    Maybe.andThen (tokenize str) parseTokens
