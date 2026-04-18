{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad (when)
import Data.List (transpose)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import qualified Data.Scientific as Scientific
import qualified Data.Set as Set

import Data.Text (Text)

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (..),
    object,
    withText,
    withObject,
    (.:),
    (.=)
  )

-- Definição de tipos para representar o State
newtype State = State {aState :: Text} deriving (Eq, Ord, Show)

instance FromJSON State where
  parseJSON :: Value -> Y.Parser State
  parseJSON v = State <$> parseTextLike v


-- Definição de tipos para representar o Symbol
newtype Symbol = Symbol {aSymbol :: Text} deriving (Eq, Show)

instance FromJSON Symbol where
  parseJSON :: Value -> Y.Parser Symbol
  parseJSON v = Symbol <$> parseTextLike v

-- Definição de tipos para representar o tipo do autômato (DFA, NFA, NFAe)
data AutomatonType = TypeDFA | TypeNFA | TypeNFAE deriving (Eq, Show)

instance FromJSON AutomatonType where
  parseJSON :: Value -> Y.Parser AutomatonType
  parseJSON = withText "type" $ \t ->
    case Text.toLower t of
      "dfa" -> pure TypeDFA
      "nfa" -> pure TypeNFA
      "nfae" -> pure TypeNFAE
      _ -> fail "Invalid type. Expected one of: dfa, nfa, nfae"

instance ToJSON AutomatonType where
  toJSON :: AutomatonType -> Value
  toJSON t =
    String $ case t of
      TypeDFA -> "dfa"
      TypeNFA -> "nfa"
      TypeNFAE -> "nfae"

-- Definição de tipos para representar uma transição do autômato
data Transition = Transition
  { tFrom :: State,
    tSymbol :: Symbol,
    tTo :: [State]
  } deriving (Eq, Show)

instance FromJSON Transition where
  parseJSON :: Value -> Y.Parser Transition
  parseJSON = withObject "Transition" $ \o ->
    Transition
      <$> o .: "from"
      <*> o .: "symbol"
      <*> o .: "to"
  
instance ToJSON Transition where
  toJSON :: Transition -> Value
  toJSON tr =
    object
      [ "from" .= aState (tFrom tr),
        "symbol" .= aSymbol (tSymbol tr),
        "to" .= map aState (tTo tr)
      ]

-- Definição de tipos para representar o autômato completo
data Automaton = Automaton
  { aType :: AutomatonType,
    aAlphabet :: [Symbol],
    aStates :: [State],
    aInitialState :: State,
    aFinalStates :: [State],
    aTransitions :: [Transition]
  } deriving (Show)

instance FromJSON Automaton where
  parseJSON :: Value -> Y.Parser Automaton
  parseJSON = withObject "Automaton" $ \o ->
    Automaton
      <$> o .: "type"
      <*> o .: "alphabet"
      <*> o .: "states"
      <*> o .: "initial_state"
      <*> o .: "final_states"
      <*> o .: "transitions"

instance ToJSON Automaton where
  toJSON :: Automaton -> Value
  toJSON automaton =
    object
      [ "type" .= aType automaton,
        "alphabet" .= map aSymbol (aAlphabet automaton),
        "states" .= map aState (aStates automaton),
        "initial_state" .= aState (aInitialState automaton),
        "final_states" .= map aState (aFinalStates automaton),
        "transitions" .= map toJSON (aTransitions automaton)
      ]


parseTextLike :: Value -> Y.Parser Text
parseTextLike (String t) = pure t
parseTextLike (Number n) = pure (numberToText n)
parseTextLike _ = fail "Esperado texto ou numero"

numberToText :: Scientific.Scientific -> Text
numberToText n =
  case Scientific.floatingOrInteger n :: Either Double Integer of
    Right i -> Text.pack (show i)
    Left _ -> Text.pack (Scientific.formatScientific Scientific.Generic Nothing n)

-- Remove tudo após '#' em cada linha do content
stripCommentsFromContent :: Text.Text -> Text.Text
stripCommentsFromContent =
  Text.unlines
  . filter (not . Text.null)
  . map (Text.stripEnd . fst . Text.breakOn "#")
  . Text.lines

-- Função para validar o campo 'type' do autômato que deve ser um dos seguintes: 'dfa', 'nfa', 'nfae' (case-insensitive)
validateAutomatonType :: Automaton -> Either String Automaton
validateAutomatonType automaton
  | aType automaton `elem` validTypes = Right automaton
  | otherwise =
      Left
        ( "Campo 'type' invalido: "
            ++ show (aType automaton)
            ++ ". Valores permitidos: dfa, nfa, nfae"
        )
  where
    validTypes = [TypeDFA, TypeNFA, TypeNFAE]

-- Função para validar que 'states' não é uma lista vazia
validateStatesNonEmpty :: Automaton -> Either String Automaton
validateStatesNonEmpty automaton
  | null (aStates automaton) = Left "Campo 'states' invalido: a lista nao pode ser vazia"
  | otherwise = Right automaton

-- Função para validar que 'initial_state' pertence à lista de 'states'
validateInitialState :: Automaton -> Either String Automaton
validateInitialState automaton
  | aInitialState automaton `elem` aStates automaton = Right automaton
  | otherwise =
      Left
        ( "Campo 'initial_state' invalido: "
            ++ Text.unpack (aState (aInitialState automaton))
            ++ " nao pertence a lista 'states'"
        )

-- Função para validar que 'final_states' é subconjunto de 'states'
validateFinalStates :: Automaton -> Either String Automaton
validateFinalStates automaton
  | all (`elem` aStates automaton) (aFinalStates automaton) = Right automaton
  | otherwise =
      Left
        ( "Campo 'final_states' invalido: "
            ++ show (filter (`notElem` aStates automaton) (aFinalStates automaton))
            ++ " nao pertencem a lista 'states'"
        )

-- Função para validar que existe ao menos uma transação
validateTransitionsNonEmpty :: Automaton -> Either String Automaton
validateTransitionsNonEmpty automaton
  | null (aTransitions automaton) = Left "Campo 'transitions' invalido: a lista nao pode ser vazia"
  | otherwise = Right automaton

-- Função para validar cada transição:
-- 1) 'from' pertence a 'states'
-- 2) 'symbol' pertence a 'alphabet'
-- 3) para DFA/NFA, 'to' é subconjunto de 'states'; para NFAe, destinos podem ser descobertos via fecho-epsilon
validateTransitions :: Automaton -> Either String Automaton
validateTransitions automaton =
  case firstTransitionError (zip [1 :: Int ..] (aTransitions automaton)) of
    Just err -> Left err
    Nothing -> Right automaton
  where
    states = aStates automaton
    alphabet = aAlphabet automaton

    firstTransitionError [] = Nothing
    firstTransitionError ((idx, tr) : rest)
      | tFrom tr `notElem` states =
          Just
            ( "Transicao "
                ++ show idx
                ++ " invalida: 'from' = "
          ++ Text.unpack (aState (tFrom tr))
                ++ " nao pertence a lista 'states'"
            )
      | tSymbol tr `notElem` alphabet =
          Just
            ( "Transicao "
                ++ show idx
                ++ " invalida: 'symbol' = "
          ++ Text.unpack (aSymbol (tSymbol tr))
                ++ " nao pertence a lista 'alphabet'"
            )
        | aType automaton /= TypeNFAE && not (all (`elem` states) (tTo tr)) =
          Just
            ( "Transicao "
                ++ show idx
                ++ " invalida: destinos "
                ++ show (filter (`notElem` states) (tTo tr))
                ++ " nao pertencem a lista 'states'"
            )
      | otherwise = firstTransitionError rest

-- Função para normalizar o alfabeto de um autômato do tipo NFAe, garantindo que "epsilon" esteja presente
normalizeAlphabetForNfae :: Automaton -> Automaton
normalizeAlphabetForNfae automaton
  | aType automaton == TypeNFAE && Symbol "epsilon" `notElem` aAlphabet automaton =
      automaton {aAlphabet = aAlphabet automaton ++ [Symbol "epsilon"]}
  | otherwise = automaton

-- Função para calcular os estados alcançáveis a partir de um estado dado seguindo apenas transições com "epsilon"
epsilonTargets :: Automaton -> State -> [State]
epsilonTargets automaton state =
  concatMap tTo [tr | tr <- aTransitions automaton, tFrom tr == state, tSymbol tr == Symbol "epsilon"]

-- Função para calcular o fecho-epsilon de um estado, ou seja, 
-- o conjunto de estados que podem ser alcançados a partir de um estado dado seguindo apenas transições com "epsilon", 
-- incluindo o próprio estado
epsilonClosure :: Automaton -> State -> [State]
epsilonClosure automaton start =
  reverse (go Set.empty [start] [])
  where
    go _ [] acc = acc
    go seen (current : rest) acc
      | Set.member current seen = go seen rest acc
      | otherwise =
          let next = epsilonTargets automaton current
           in go (Set.insert current seen) (next ++ rest) (current : acc)

uniqueStatesOrdered :: [State] -> [State]
uniqueStatesOrdered = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

splitCombinedState :: State -> [State]
splitCombinedState state =
  case Text.splitOn "," (aState state) of
    [] -> [state]
    parts -> map (State . Text.strip) parts

closureForStoredState :: Automaton -> State -> [State]
closureForStoredState automaton state =
  uniqueStatesOrdered (splitCombinedState state >>= epsilonClosure automaton)

stateFromStateList :: [State] -> State
stateFromStateList states = State (formatTargets (uniqueStatesOrdered states))

-- Função para construir a tabela de fecho-epsilon para cada estado do autômato
buildClosureTable :: Automaton -> ([(State, [State])], [State])
buildClosureTable automaton =
  (rows, knownStates)
  where
    initialStates = aStates automaton

    rows =
      [ (state, closureStates)
      | state <- initialStates
      , let closureStates = closureForStoredState automaton state
      ]

    discoveredAtomicStates =
      uniqueStatesOrdered
        [ s
        | (_, closureStates) <- rows
        , s <- closureStates
        , s `notElem` initialStates
        ]

    discoveredClosureStates =
      uniqueStatesOrdered
        [ closureState
        | (_, closureStates) <- rows
        , let closureState = stateFromStateList closureStates
        , closureState `notElem` initialStates
        ]

    knownStates = initialStates ++ discoveredAtomicStates ++ discoveredClosureStates

-- Função para imprimir a tabela de fecho-epsilon de forma legível
printClosureTable :: [(State, [State])] -> IO ()
printClosureTable rows = do
  let header = ["Estado corrente", "Fecho-e"]
      matrixRows =
        [ [aState state, formatTargets closureStates]
        | (state, closureStates) <- rows
        ]
  renderAlignedTable header matrixRows

printStatesTable :: [State] -> IO ()
printStatesTable states = do
  let header = ["Lista de estados"]
      matrixRows =
        if null states
          then [["∅"]]
          else [[aState state] | state <- states]
  renderAlignedTable header matrixRows

-- Função para calcular as transições para cada símbolo do alfabeto (exceto "epsilon") a partir de um estado, 
-- considerando o fecho-epsilon
buildTransitionsTable :: Automaton -> [(State, [State], [(Symbol, [State])])]
buildTransitionsTable automaton =
  go initialStates initialStates []
  where
    initialStates = aStates automaton
    symbols = [symbol | symbol <- aAlphabet automaton, symbol /= Symbol "epsilon"]

    go _ [] rows = rows
    go knownStates (current : pendingStates) rows =
      let closureStates = closureForStoredState automaton current
          transitionsBySymbol =
            [ (symbol, targetStates closureStates symbol)
            | symbol <- symbols
            ]
          row = (current, closureStates, transitionsBySymbol)
          discoveredStates =
            uniqueStatesOrdered
              [ combinedState
              | (_, targets) <- transitionsBySymbol
              , not (null targets)
              , let combinedState = stateFromStateList targets
              , combinedState `notElem` knownStates
              ]
          updatedStates = knownStates ++ discoveredStates
          updatedPending = pendingStates ++ discoveredStates
       in go updatedStates updatedPending (rows ++ [row])

    targetStates closureStates symbol =
      uniqueStatesOrdered
        ( closureStates >>= \s ->
            concat [tTo tr | tr <- aTransitions automaton, tFrom tr == s, tSymbol tr == symbol] >>= closureForStoredState automaton
        )

printTransitionsTable :: [(State, [State], [(Symbol, [State])])] -> IO ()
printTransitionsTable rows = do
  let symbols = case rows of
        [] -> []
        ((_, _, transitionsBySymbol) : _) -> map fst transitionsBySymbol
      header = ["Estado corrente"] ++ map aSymbol symbols
      matrixRows =
        [ [aState state]
            ++ map (formatTargets . snd) transitionsBySymbol
        | (state, _, transitionsBySymbol) <- rows
        ]
  renderAlignedTable header matrixRows

canonicalStateSet :: [State] -> [State]
canonicalStateSet = Set.toAscList . Set.fromList

-- Para a conversao NFA -> DFA, cada estado do DFA eh um conjunto de estados do NFA.
-- Esta funcao constroi recursivamente a tabela de transicoes por simbolo para cada estado-conjunto descoberto.
buildNfaToDfaStep1Table :: Automaton -> [([State], [(Symbol, [State])])]
buildNfaToDfaStep1Table automaton =
  go [initialSet] [initialSet] []
  where
    symbols = [symbol | symbol <- aAlphabet automaton, symbol /= Symbol "epsilon"]
    initialSet = canonicalStateSet [aInitialState automaton]

    go _ [] rows = rows
    go knownSets (currentSet : pendingSets) rows =
      let transitionsBySymbol =
            [ (symbol, nextSet currentSet symbol)
            | symbol <- symbols
            ]
          row = (currentSet, transitionsBySymbol)
          discoveredSets =
            uniqueStateSets
              [ targets
              | (_, targets) <- transitionsBySymbol
              , targets `notElem` knownSets
              ]
          updatedKnown = knownSets ++ discoveredSets
          updatedPending = pendingSets ++ discoveredSets
       in go updatedKnown updatedPending (rows ++ [row])

    nextSet currentSet symbol =
      canonicalStateSet
        [ target
        | source <- currentSet
        , tr <- aTransitions automaton
        , tFrom tr == source
        , tSymbol tr == symbol
        , target <- tTo tr
        ]

    uniqueStateSets = goUnique Set.empty
      where
        goUnique _ [] = []
        goUnique seen (x : xs)
          | Set.member x seen = goUnique seen xs
          | otherwise = x : goUnique (Set.insert x seen) xs

printNfaToDfaStep1Table :: [([State], [(Symbol, [State])])] -> IO ()
printNfaToDfaStep1Table rows = do
  let visibleRows = [row | row@(currentSet, _) <- rows, not (null currentSet)]
      symbols = case rows of
        [] -> []
        ((_, transitionsBySymbol) : _) -> map fst transitionsBySymbol
      header = ["Estado corrente"] ++ map aSymbol symbols
      matrixRows =
        [ [formatTargets currentSet]
            ++ map (formatTargets . snd) transitionsBySymbol
        | (currentSet, transitionsBySymbol) <- visibleRows
        ]
  renderAlignedTable header matrixRows

buildNfaToDfaStep2MarkedTable ::
  Automaton ->
  [([State], [(Symbol, [State])])] ->
  [(Text, [State], [(Symbol, [State])])]
buildNfaToDfaStep2MarkedTable automaton rows =
  [ (markerFor currentSet, currentSet, transitionsBySymbol)
  | (currentSet, transitionsBySymbol) <- rows
  , not (null currentSet)
  ]
  where
    nfaFinalStates = aFinalStates automaton
    initialSet = canonicalStateSet [aInitialState automaton]

    markerFor currentSet = initialMark <> finalMark
      where
        initialMark = if currentSet == initialSet then "->" else ""
        finalMark = if any (`elem` nfaFinalStates) currentSet then "*" else ""

printNfaToDfaStep2MarkedTable ::
  [(Text, [State], [(Symbol, [State])])] ->
  IO ()
printNfaToDfaStep2MarkedTable rows = do
  let symbols = case rows of
        [] -> []
        ((_, _, transitionsBySymbol) : _) -> map fst transitionsBySymbol
      header = ["", "Estado corrente"] ++ map aSymbol symbols
      matrixRows =
        [ [marker, formatTargets currentSet]
            ++ map (formatTargets . snd) transitionsBySymbol
        | (marker, currentSet, transitionsBySymbol) <- rows
        ]
  renderAlignedTable header matrixRows

stateSetToDfaState :: [State] -> State
stateSetToDfaState [] = State "EMPTY"
stateSetToDfaState states =
  State (Text.intercalate "_" (map aState (canonicalStateSet states)))

buildConvertedDfa ::
  Automaton ->
  [([State], [(Symbol, [State])])] ->
  [(Text, [State], [(Symbol, [State])])] ->
  Automaton
buildConvertedDfa sourceAutomaton step1Rows step2MarkedRows =
  Automaton
    { aType = TypeDFA,
      aAlphabet = [symbol | symbol <- aAlphabet sourceAutomaton, symbol /= Symbol "epsilon"],
      aStates = convertedStates,
      aInitialState = stateSetToDfaState [aInitialState sourceAutomaton],
      aFinalStates = convertedFinalStates,
      aTransitions = convertedTransitions
    }
  where
    originalFinalStates = aFinalStates sourceAutomaton

    convertedStates =
      uniqueStatesOrdered
        [ stateSetToDfaState currentSet
        | (currentSet, _) <- step1Rows
        ]

    convertedFinalStates =
      uniqueStatesOrdered
        [ stateSetToDfaState currentSet
        | (_marker, currentSet, _transitionsBySymbol) <- step2MarkedRows
        , any (`elem` originalFinalStates) currentSet
        ]

    convertedTransitions =
      [ Transition
          { tFrom = stateSetToDfaState currentSet,
            tSymbol = symbol,
            tTo = [stateSetToDfaState targetSet]
          }
      | (currentSet, transitionsBySymbol) <- step1Rows,
        (symbol, targetSet) <- transitionsBySymbol,
        stateSetToDfaState currentSet `elem` convertedStates
      ]

buildMarkedTransitionsTable ::
  Automaton ->
  [(Text, State, [State], [(Symbol, [State])])]
buildMarkedTransitionsTable automaton =
  [ (markerFor state transitionsBySymbol, state, closureStates, transitionsBySymbol)
  | (state, closureStates, transitionsBySymbol) <- buildTransitionsTable automaton
  ]
  where
    finalStates = aFinalStates automaton

    markerFor state _transitionsBySymbol =
      initialMark <> finalMark
      where
        initialMark = if state == aInitialState automaton then "->" else ""
        finalMark = if reachesFinalByClosure state then "*" else ""

    reachesFinalByClosure state =
      any (`elem` finalStates) (closureForStoredState automaton state)

printMarkedTransitionsTable ::
  [(Text, State, [State], [(Symbol, [State])])] ->
  IO ()
printMarkedTransitionsTable rows = do
  let symbols = case rows of
        [] -> []
        ((_, _, _, transitionsBySymbol) : _) -> map fst transitionsBySymbol
      header = ["", "Estado corrente", "Fecho-e"] ++ map aSymbol symbols
      matrixRows =
        [ [ marker,
            aState state,
            formatTargets closureStates
          ]
            ++ map (formatTargets . snd) transitionsBySymbol
        | (marker, state, closureStates, transitionsBySymbol) <- rows
        ]
  renderAlignedTable header matrixRows

buildConvertedNfa ::
  Automaton ->
  [(State, [State], [(Symbol, [State])])] ->
  [(Text, State, [State], [(Symbol, [State])])] ->
  Automaton
buildConvertedNfa sourceAutomaton transitionsRows markedRows =
  Automaton
    { aType = TypeNFA,
      aAlphabet = [symbol | symbol <- aAlphabet sourceAutomaton, symbol /= Symbol "epsilon"],
      aStates = originalStates,
      aInitialState = aInitialState sourceAutomaton,
      aFinalStates = convertedFinalStates,
      aTransitions = convertedTransitions
    }
  where
    originalStates = [state | (_, state, _, _) <- markedRows]
    originalFinalStates = aFinalStates sourceAutomaton

    convertedFinalStates =
      uniqueStatesOrdered
        [ state
        | (_, state, closureStates, _) <- markedRows
        , any (`elem` originalFinalStates) closureStates
        ]

    convertedTransitions =
      [ Transition
          { tFrom = state,
            tSymbol = symbol,
            tTo = targetStates
          }
      | (state, _, transitionsBySymbol) <- transitionsRows,
        (symbol, targetStates) <- transitionsBySymbol,
        not (null targetStates)
      ]

formatTargets :: [State] -> Text
formatTargets [] = "∅"
formatTargets states = Text.intercalate "," (map aState states)

renderAlignedTable :: [Text] -> [[Text]] -> IO ()
renderAlignedTable header matrixRows = do
  let matrix = header : matrixRows
      widths = map (maximum . map Text.length) (transpose matrix)
      separator = Text.intercalate "-+-" [Text.replicate w "-" | w <- widths]

  TIO.putStrLn (renderRow widths header)
  TIO.putStrLn separator
  mapM_ (TIO.putStrLn . renderRow widths) matrixRows
  where
    renderRow widths cells =
      Text.intercalate " | " (zipWith padRight widths cells)

    padRight width value =
      value <> Text.replicate (width - Text.length value) " "

renderAutomatonYaml :: Automaton -> Text
renderAutomatonYaml automaton =
  Text.unlines
    ( [ "type: " <> automatonTypeText (aType automaton),
        "alphabet: " <> renderSymbolsInline (aAlphabet automaton),
        "states: " <> renderStatesInline (aStates automaton),
        "initial_state: " <> aState (aInitialState automaton),
        "final_states: " <> renderStatesInline (aFinalStates automaton),
        "transitions:"
      ]
        ++ concatMap renderTransitionLines (aTransitions automaton)
    )
  where
    automatonTypeText TypeDFA = "dfa"
    automatonTypeText TypeNFA = "nfa"
    automatonTypeText TypeNFAE = "nfae"

    displayStateName stateName
      | stateName == "EMPTY" = "∅"
      | otherwise = stateName

    renderYamlText t
      | needsQuotes t = "\"" <> escapeYamlText t <> "\""
      | otherwise = t

    needsQuotes t = Text.any (`elem` [',', '[', ']', '{', '}', ':']) t || Text.any (`elem` [' ', '\t']) t

    escapeYamlText = Text.concatMap escapeChar

    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = Text.singleton c

    renderSymbolsInline symbols =
      "[" <> Text.intercalate ", " (map aSymbol symbols) <> "]"

    renderStatesInline states =
      "[" <> Text.intercalate ", " (map (renderYamlText . displayStateName . aState) states) <> "]"

    renderTransitionLines transition =
      [ "- from: " <> renderYamlText (displayStateName (aState (tFrom transition))),
        "  symbol: " <> aSymbol (tSymbol transition),
        "  to: " <> renderStatesInline (tTo transition)
      ]

main :: IO ()
main = do

  -- Lê o arquivo de entrada a partir dos argumentos da linha de comando
  args <- getArgs
  content <- case args of
    [filePath] -> TIO.readFile filePath
    _ -> die "Usage: lab01-part1 <input-file.yaml>"
  let strippedContent = stripCommentsFromContent content

  -- Converte o conteúdo YAML para o tipo de dado Automaton, tratando erros de parsing
  automaton <- case Y.decodeEither' (TE.encodeUtf8 strippedContent) of
    Left err -> die (Y.prettyPrintParseException err)
    Right a -> pure a

  -- Normaliza o alfabeto para autômatos do tipo NFAe, garantindo que "epsilon" esteja presente
  let normalizedAutomaton = normalizeAlphabetForNfae automaton

  -- Bloco de validações do autômato, onde cada validação retorna Either String Automaton. 
  -- Em caso de erro, a mensagem é exibida e o programa termina. 
  -- Caso contrário, o autômato é passado para a próxima validação.
  _ <- case validateAutomatonType normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton

  _ <- case validateStatesNonEmpty normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton

  _ <- case validateInitialState normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton

  _ <- case validateFinalStates normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton

  _ <- case validateTransitionsNonEmpty normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton

  _ <- case validateTransitions normalizedAutomaton of
    Left msg -> die msg
    Right okAutomaton -> pure okAutomaton


  -- Verifica se existe transição com movimento vazio ("epsilon").
  -- Nesse caso, trata-se de NFAe.
  -- Vamos converter o autômato para NFA, removendo as transições com "epsilon"

  -- Primeiro passo: Calcular o fecho-epsilon de cada estado, ou seja, 
  -- o conjunto de estados que podem ser alcançados a partir de um estado dado 
  -- seguindo apenas transições com "epsilon", incluindo o próprio estado, 
  -- iniciando a partir do estado inicial.

  when (aType normalizedAutomaton == TypeNFAE) $ do
      let exportedType = TypeNFA
      putStrLn "================================="
      putStrLn "Passo 1: Recebido um autômato do tipo NFAe. Calculando o fecho-epsilon de cada estado..."
      let (closureTable, expandedStates) = buildClosureTable normalizedAutomaton
      let expandedAutomaton = normalizedAutomaton {aStates = expandedStates}
      printClosureTable closureTable
      -- putStrLn "Lista de estados atualizada:"
      -- printStatesTable expandedStates
      putStrLn "--------------------------------"
      putStrLn "Passo 2: Para cada simbolo do alfabeto (exceto 'epsilon'), calcular as transições"
      let transitionsTable = buildTransitionsTable normalizedAutomaton
      let originalStates = aStates normalizedAutomaton
      let transitionsTableForStep2 = [row | row@(state, _, _) <- transitionsTable, state `elem` originalStates]
      printTransitionsTable transitionsTableForStep2
      putStrLn "--------------------------------"
      putStrLn "Passo 3: Marcar na tabela os estados iniciais e finais"
      let markedTransitionsTable = buildMarkedTransitionsTable expandedAutomaton
      let closureFromStep1 state =
            case lookup state closureTable of
              Just closureStates -> closureStates
              Nothing -> []
      let markedTransitionsTableForStep3 = [(marker, state, closureFromStep1 state, transitionsBySymbol) | (marker, state, _, transitionsBySymbol) <- markedTransitionsTable, state `elem` originalStates]
      printMarkedTransitionsTable markedTransitionsTableForStep3
      putStrLn "--------------------------------"
      putStrLn "Passo 4: Apresentação do autômato convertido para o formato para NFA"
      let convertedNfa = buildConvertedNfa normalizedAutomaton transitionsTableForStep2 markedTransitionsTableForStep3
      let convertedNfaYaml = renderAutomatonYaml convertedNfa
      let outputFilePath = "nfae-nfa.yaml"
      TIO.putStrLn convertedNfaYaml
      TIO.writeFile outputFilePath convertedNfaYaml
      putStrLn ("Arquivo YAML gerado em: " ++ outputFilePath)


  when (aType normalizedAutomaton == TypeNFA) $ do
      putStrLn "Recebido um autômato do tipo NFA. Exportando o autômato no formato de DFA..."
      let exportedType = TypeDFA
      putStrLn "================================="
      putStrLn "Passo 1: Recebido um autômato do tipo NFA (sem movimentos vazios)."
      putStrLn "Para cada novo estado do DFA (representado por um conjunto de estados do NFA),"
      putStrLn "calcular para onde ele vai com cada símbolo do alfabeto, unindo as transições"
      putStrLn "individuais de todos os estados do conjunto. Cada resultado gera um novo"
      putStrLn "estado-conjunto candidato do DFA, que deve ser processado recursivamente."
      let step1Table = buildNfaToDfaStep1Table normalizedAutomaton
      printNfaToDfaStep1Table step1Table
      putStrLn "--------------------------------"
      putStrLn "Passo 2: Marcar na tabela os estados iniciais e finais do DFA."
      putStrLn "O estado inicial do DFA e o conjunto que contem o estado inicial do NFA."
      putStrLn "Um estado do DFA e de aceitacao quando contem pelo menos um estado"
      putStrLn "de aceitacao do NFA original."
      let step2MarkedTable = buildNfaToDfaStep2MarkedTable normalizedAutomaton step1Table
      printNfaToDfaStep2MarkedTable step2MarkedTable
      putStrLn "--------------------------------"
      putStrLn "Passo 3: Apresentacao do automato convertido para o formato de DFA"
      let convertedDfa = buildConvertedDfa normalizedAutomaton step1Table step2MarkedTable
      let convertedDfaYaml = renderAutomatonYaml convertedDfa
      let outputFilePath = "nfa-dfa.yaml"
      TIO.putStrLn convertedDfaYaml
      TIO.writeFile outputFilePath convertedDfaYaml
      putStrLn ("Arquivo YAML gerado em: " ++ outputFilePath)
      