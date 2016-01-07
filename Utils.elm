module Utils where

-- this is essentially a lazy version of Maybe.oneOf
tryAll : List (() -> Maybe a) -> Maybe a
tryAll lst =
  case lst of
    [] -> Nothing
    (x :: xs) ->
      case x () of
        Just success -> Just success
        Nothing -> tryAll xs


requireAll : List (() -> Maybe a) -> Maybe (List a)
requireAll lst =
  case lst of
    [] -> Just []
    (x :: xs) ->
      case x () of
        Just success -> Maybe.map ((::) success) <| requireAll xs
        Nothing -> Nothing


greedyIterate : (a -> Maybe (b, a)) -> a -> (List b, a)
greedyIterate f state =
  case f state of
    Just (result, state') ->
        let (results, finalState) = greedyIterate f state'
        in (result :: results, finalState)
    Nothing -> ([], state)
