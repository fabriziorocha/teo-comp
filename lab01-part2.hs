{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad.State.Strict (State, evalState, state)
import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), FromJSON (parseJSON), ToJSON (toJSON), Value, object, withObject)
import Data.Aeson ((.=))
import Data.List (sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import System.Environment (getArgs)
import System.Exit (die)

newtype AState = AState {aState :: Text} deriving (Eq, Ord, Show)
newtype Symbol = Symbol {aSymbol :: Text} deriving (Eq, Ord, Show)

data AutomatonType = TypeNFAE deriving (Eq, Show)

data Transition = Transition
  { tFrom :: AState,
    tSymbol :: Symbol,
    tTo :: [AState]
  }
  deriving (Eq, Show)

data Automaton = Automaton
  { aType :: AutomatonType,
    aAlphabet :: [Symbol],
    aStates :: [AState],
    aInitialState :: AState,
    aFinalStates :: [AState],
    aTransitions :: [Transition]
  }
  deriving (Eq, Show)

instance ToJSON AState where
  toJSON :: AState -> Value
  toJSON = toJSON . aState

instance ToJSON Symbol where
  toJSON :: Symbol -> Value
  toJSON = toJSON . aSymbol

instance ToJSON AutomatonType where
  toJSON :: AutomatonType -> Value
  toJSON TypeNFAE = toJSON ("nfae" :: Text)

instance ToJSON Transition where
  toJSON :: Transition -> Value
  toJSON tr =
    object
      [ "from" .= aState (tFrom tr),
        "symbol" .= aSymbol (tSymbol tr),
        "to" .= map aState (tTo tr)
      ]

instance ToJSON Automaton where
  toJSON :: Automaton -> Value
  toJSON aut =
    object
      [ "type" .= aType aut,
        "alphabet" .= map aSymbol (aAlphabet aut),
        "states" .= map aState (aStates aut),
        "initial_state" .= aState (aInitialState aut),
        "final_states" .= map aState (aFinalStates aut),
        "transitions" .= map toJSON (aTransitions aut)
      ]

data RegexInput = RegexInput
  { riExpression :: Text
  }
  deriving (Show)

instance FromJSON RegexInput where
  parseJSON :: Value -> Y.Parser RegexInput
  parseJSON = withObject "RegexInput" $ \o -> do
    mExpression <- o .:? "expression"
    mRegex <- o .:? "regex"
    expression <-
      case mExpression <|> mRegex of
        Just txt -> pure txt
        Nothing -> fail "Expected key 'expression' or 'regex' in the input YAML"
    pure (RegexInput expression)

data Regex
  = RSymbol Text
  | REpsilon
  | RConcat Regex Regex
  | RUnion Regex Regex
  | RStar Regex
  | RPlus Regex
  | ROptional Regex
  deriving (Eq, Show)

data Token
  = TLParen
  | TRParen
  | TUnion
  | TStar
  | TPlus
  | TQuestion
  | TEpsilon
  | TSymbol Text
  deriving (Eq, Show)

tokenize :: Text -> Either String [Token]
tokenize input = go (Text.unpack input)
  where
    go [] = Right []
    go (c : cs)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' = go cs
      | c == '(' = (TLParen :) <$> go cs
      | c == ')' = (TRParen :) <$> go cs
      | c == '|' = (TUnion :) <$> go cs
      | c == '*' = (TStar :) <$> go cs
      | c == '+' = (TPlus :) <$> go cs
      | c == '?' = (TQuestion :) <$> go cs
      | startsWithEpsilon (c : cs) = (TEpsilon :) <$> go (drop 7 (c : cs))
      | isMeta c = Left ("Unexpected token: " ++ [c])
      | otherwise = (TSymbol (Text.singleton c) :) <$> go cs

    isMeta c = c `elem` ['(', ')', '|', '*', '+', '?']

    startsWithEpsilon chars = take 7 chars == "epsilon"

parseRegex :: [Token] -> Either String Regex
parseRegex toks = do
  (ast, rest) <- parseUnion toks
  case rest of
    [] -> Right ast
    _ -> Left "Unexpected tokens after end of expression"

parseUnion :: [Token] -> Either String (Regex, [Token])
parseUnion toks = do
  (lhs, rest) <- parseConcat toks
  parseUnionTail lhs rest
  where
    parseUnionTail acc (TUnion : more) = do
      (rhs, rest) <- parseConcat more
      parseUnionTail (RUnion acc rhs) rest
    parseUnionTail acc rest = Right (acc, rest)

parseConcat :: [Token] -> Either String (Regex, [Token])
parseConcat toks = do
  (firstExpr, rest) <- parsePostfix toks
  parseConcatTail firstExpr rest
  where
    parseConcatTail acc allToks@(next : _)
      | startsAtom next = do
          (rhs, rest) <- parsePostfix allToks
          parseConcatTail (RConcat acc rhs) rest
    parseConcatTail acc rest = Right (acc, rest)

    startsAtom TLParen = True
    startsAtom TEpsilon = True
    startsAtom (TSymbol _) = True
    startsAtom _ = False

parsePostfix :: [Token] -> Either String (Regex, [Token])
parsePostfix toks = do
  (baseExpr, rest) <- parseAtom toks
  applyPostfix baseExpr rest
  where
    applyPostfix acc (TStar : more) = applyPostfix (RStar acc) more
    applyPostfix acc (TPlus : more) = applyPostfix (RPlus acc) more
    applyPostfix acc (TQuestion : more) = applyPostfix (ROptional acc) more
    applyPostfix acc rest = Right (acc, rest)

parseAtom :: [Token] -> Either String (Regex, [Token])
parseAtom [] = Left "Unexpected end of input"
parseAtom (TSymbol s : rest) = Right (RSymbol s, rest)
parseAtom (TEpsilon : rest) = Right (REpsilon, rest)
parseAtom (TLParen : rest) = do
  (inside, afterInside) <- parseUnion rest
  case afterInside of
    (TRParen : remaining) -> Right (inside, remaining)
    _ -> Left "Missing closing ')'"
parseAtom (tok : _) = Left ("Unexpected token in atom: " ++ show tok)

data RawTransition = RawTransition
  { rtFrom :: Int,
    rtSymbol :: Text,
    rtTo :: Int
  }
  deriving (Eq, Show)

data Fragment = Fragment
  { fStart :: Int,
    fEnd :: Int,
    fTransitions :: [RawTransition],
    fAlphabet :: Set.Set Text
  }
  deriving (Eq, Show)

freshState :: State Int Int
freshState = state (\n -> (n, n + 1))

compileRegex :: Regex -> State Int Fragment
compileRegex (RSymbol s) = do
  start <- freshState
  end <- freshState
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions = [RawTransition start s end],
        fAlphabet = Set.singleton s
      }
compileRegex REpsilon = do
  start <- freshState
  end <- freshState
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions = [RawTransition start "epsilon" end],
        fAlphabet = Set.empty
      }
compileRegex (RConcat r1 r2) = do
  f1 <- compileRegex r1
  f2 <- compileRegex r2
  pure
    Fragment
      { fStart = fStart f1,
        fEnd = fEnd f2,
        fTransitions =
          fTransitions f1
            ++ [RawTransition (fEnd f1) "epsilon" (fStart f2)]
            ++ fTransitions f2,
        fAlphabet = Set.union (fAlphabet f1) (fAlphabet f2)
      }
compileRegex (RUnion r1 r2) = do
  start <- freshState
  end <- freshState
  f1 <- compileRegex r1
  f2 <- compileRegex r2
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions =
          [ RawTransition start "epsilon" (fStart f1),
            RawTransition start "epsilon" (fStart f2),
            RawTransition (fEnd f1) "epsilon" end,
            RawTransition (fEnd f2) "epsilon" end
          ]
            ++ fTransitions f1
            ++ fTransitions f2,
        fAlphabet = Set.union (fAlphabet f1) (fAlphabet f2)
      }
compileRegex (RStar r) = do
  start <- freshState
  end <- freshState
  f <- compileRegex r
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions =
          [ RawTransition start "epsilon" end,
            RawTransition start "epsilon" (fStart f),
            RawTransition (fEnd f) "epsilon" (fStart f),
            RawTransition (fEnd f) "epsilon" end
          ]
            ++ fTransitions f,
        fAlphabet = fAlphabet f
      }
compileRegex (RPlus r) = do
  start <- freshState
  end <- freshState
  f <- compileRegex r
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions =
          [ RawTransition start "epsilon" (fStart f),
            RawTransition (fEnd f) "epsilon" (fStart f),
            RawTransition (fEnd f) "epsilon" end
          ]
            ++ fTransitions f,
        fAlphabet = fAlphabet f
      }
compileRegex (ROptional r) = do
  start <- freshState
  end <- freshState
  f <- compileRegex r
  pure
    Fragment
      { fStart = start,
        fEnd = end,
        fTransitions =
          [ RawTransition start "epsilon" end,
            RawTransition start "epsilon" (fStart f),
            RawTransition (fEnd f) "epsilon" end
          ]
            ++ fTransitions f,
        fAlphabet = fAlphabet f
      }

stateName :: Int -> AState
stateName n = AState ("q" <> Text.pack (show n))

toAutomaton :: Fragment -> Automaton
toAutomaton frag =
  Automaton
    { aType = TypeNFAE,
      aAlphabet = map Symbol (sort (Set.toList (fAlphabet frag))),
      aStates = states,
      aInitialState = stateName (fStart frag),
      aFinalStates = [stateName (fEnd frag)],
      aTransitions = transitions
    }
  where
    stateIds =
      Set.toList
        ( Set.fromList
            ([fStart frag, fEnd frag] ++ concatMap (\tr -> [rtFrom tr, rtTo tr]) (fTransitions frag))
        )

    states = map stateName (sort stateIds)

    transitions =
      [ Transition
          { tFrom = stateName (rtFrom tr),
            tSymbol = Symbol (rtSymbol tr),
            tTo = [stateName (rtTo tr)]
          }
      | tr <- fTransitions frag
      ]

renderAutomatonYaml :: Automaton -> Text
renderAutomatonYaml aut =
  Text.unlines
    ( [ "type: nfae",
        "alphabet: " <> renderSymbolsInline (aAlphabet aut),
        "states: " <> renderStatesInline (aStates aut),
        "initial_state: " <> aState (aInitialState aut),
        "final_states: " <> renderStatesInline (aFinalStates aut),
        "transitions:"
      ]
        ++ concatMap renderTransitionLines (aTransitions aut)
    )
  where
    renderSymbolsInline symbols =
      "[" <> Text.intercalate ", " (map aSymbol symbols) <> "]"

    renderStatesInline states =
      "[" <> Text.intercalate ", " (map aState states) <> "]"

    renderTransitionLines tr =
      [ "- from: " <> aState (tFrom tr),
        "  symbol: " <> aSymbol (tSymbol tr),
        "  to: " <> renderStatesInline (tTo tr)
      ]

renderAutomatonPlantUml :: Text -> Automaton -> Text
renderAutomatonPlantUml regex aut =
  Text.unlines
    ( [ "@startuml",
        "title Automato de reconhecimento para regex: " <> regex,
        "hide empty description",
        "skinparam state {",
        "  BackgroundColor White",
        "  BorderColor Black",
        "}",
        "[*] --> " <> aState (aInitialState aut)
      ]
        ++ map renderFinalState (aFinalStates aut)
        ++ concatMap renderTransition (aTransitions aut)
        ++ ["@enduml"]
    )
  where
    renderFinalState st = "state " <> aState st <> " <<accepting>>"

    renderTransition tr =
      [ aState (tFrom tr)
          <> " --> "
          <> aState target
          <> " : "
          <> escapePlantUmlLabel (aSymbol (tSymbol tr))
      | target <- tTo tr
      ]

    escapePlantUmlLabel = Text.replace "\n" "\\n"

stripCommentsFromContent :: Text -> Text
stripCommentsFromContent =
  Text.unlines
    . filter (not . Text.null)
    . map (Text.stripEnd . fst . Text.breakOn "#")
    . Text.lines

main :: IO ()
main = do
  args <- getArgs
  content <- case args of
    [filePath] -> TIO.readFile filePath
    _ -> die "Usage: lab01-part2 <input-file.yaml>"

  let strippedContent = stripCommentsFromContent content

  regexInput <-
    case Y.decodeEither' (TE.encodeUtf8 strippedContent) of
      Left err -> die (Y.prettyPrintParseException err)
      Right parsed -> pure parsed

  tokens <-
    case tokenize (riExpression regexInput) of
      Left err -> die ("Tokenizer error: " ++ err)
      Right toks -> pure toks

  regexAst <-
    case parseRegex tokens of
      Left err -> die ("Parser error: " ++ err)
      Right ast -> pure ast

  let fragment = evalState (compileRegex regexAst) 0
  let automaton = toAutomaton fragment
  let automatonYaml = renderAutomatonYaml automaton
  let automatonPlantUml = renderAutomatonPlantUml (riExpression regexInput) automaton
  let outputFilePath = "regex-nfae.yaml"
  let outputPlantUmlPath = "regex-nfae.puml"

  putStrLn "Expressao regular recebida. Gerando NFAe com construcao de Thompson..."
  TIO.writeFile outputFilePath automatonYaml
  TIO.writeFile outputPlantUmlPath automatonPlantUml
  putStrLn ("Arquivo YAML gerado em: " ++ outputFilePath)
  putStrLn ("Arquivo PlantUML gerado em: " ++ outputPlantUmlPath)
