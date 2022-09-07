{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module L0.Parser (nextStep) where

import qualified Data.Text as Text 
import Data.Text (Text) 
import Data.List
import Data.List.Index (imap)
import Data.Vector (Vector, fromList, (!?))
import Data.Void
import Flow ((|>))
import Prelude hiding(id)

import L0.Token (L0Token(..), Loc(..))
import qualified L0.Token as Token
import Parser.Expr(Expr(..))
import Parser.Meta(Meta(..))



data State =
    State { step :: Int
    , tokens :: Vector L0Token
    , numberOfTokens :: Int
    , tokenIndex :: Int
    , committed :: [Expr]
    , stack :: [L0Token]
    , messages :: [Text]
    , lineNumber :: Int
    }

data Step state a
    = Loop state
    | Done a

initWithTokens :: Int -> [L0Token] -> State
initWithTokens lineNumber_ tokens_ =
   State { step = 0
    , tokens = Data.Vector.fromList $ reverse tokens_
    , numberOfTokens = length tokens_
    , tokenIndex = 0
    , committed = []
    , stack = []
    , messages = []
    , lineNumber = lineNumber_
    }



nextStep :: State -> Step State State
nextStep state =
    case getToken state of
        Nothing ->
            if stackIsEmpty state then
                Done state

            else
                recoverFromError state

        Just token ->
            state
                |> advanceTokenIndex
                |> pushOrCommit token
                |> reduceState
                |> (\st -> State { step = 1 + step st })
                |> Loop

getToken :: State -> Maybe L0Token
getToken state = (tokens state) !? (tokenIndex state)

stackIsEmpty :: State -> Bool
stackIsEmpty state = (stack state) == []

advanceTokenIndex :: State -> State
advanceTokenIndex state = state {tokenIndex = 1 + (tokenIndex state)}


recoverFromError :: State -> Step State State
recoverFromError state = Done state

reduceState :: State -> State
reduceState state = state

pushOrCommit :: L0Token -> State -> State
pushOrCommit token state =
    case token of
        S _ _ ->
            pushOrCommit_ token state

        W _ _ ->
            pushOrCommit_ token state

        MathToken _ ->
            pushOnStack_ token state

        CodeToken _ ->
            pushOnStack_ token state

        LB _ ->
            pushOnStack_ token state

        RB _ ->
            pushOnStack_ token state


pushOnStack_ :: L0Token -> State -> State
pushOnStack_ token state =
    state { stack = token : (stack state) }


pushOrCommit_ :: L0Token -> State -> State
pushOrCommit_ token state =
    if stackIsEmpty state then
        commit token state

    else
        push token state

push :: L0Token -> State -> State
push token state =
    state { stack = token : (stack state) }


commit :: L0Token -> State -> State
commit token state =
    case stringTokenToExpr token of
        Nothing ->
            state

        Just expr ->
            state { committed = expr : (committed state) }


stringTokenToExpr :: L0Token -> Maybe Expr
stringTokenToExpr token =
    case token of
        S txt loc ->
            Just (Parser.Expr.Text txt (boostMeta 0 (Token.getIndex token) loc))

        W txt loc ->
            Just (Parser.Expr.Text txt (boostMeta 0 (Token.getIndex token) loc))

        _ ->
            Nothing


boostMeta :: Int -> Int -> Loc -> Meta
boostMeta lineNumber tokenIndex loc =
   Meta { begin = (Token.begin loc), end = (Token.end loc), index = (Token.index loc), id = makeId lineNumber tokenIndex }


makeId :: Int -> Int -> Text
makeId a b =
    (Text.pack $ show a) <> "." <> (Text.pack $ show b)
