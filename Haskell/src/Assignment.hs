module Assignment (bnfParser, generateHaskellCode, validate, ADT, getTime, parseFirstParser) where

--Custom Imports
import Control.Applicative(many, (<|>), some, asum)
import Data.Char(toUpper)

--Imports from file
import Instances (Parser (..), ParseResult (Result), ParseError (UnexpectedString))
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Parser (charTok, isNot, inlineSpaces, is, alpha, digit, eof, lower, commaTok, string, spaces, stringTok, sepBy, failed, inlineSpaces1)

{- 
    Terminal String : A type representing the terminal which can contain any kind of symbols
    NonTerminal String : A type representing the Non Terminal which can only contain alphanumeric symbols or _
    Parameter String [ADT] : A type representing the Parameter Non Terminal following the same rule as Non Terminal 
    Consecutive ADT ADT : A type representing two ADTs that are in succession
    Alternate ADT ADT : A type representing two rules that are connected by an alternate
    OneLine ADT ADT : A type representing lines of the rules being connected
    EqualSign ADT ADT : A type representing the LHS and the rule of the LHS respectively connected by an equal sign
    NormalParam String : A type representing the normal parameter in the parameter
    Alpha : A type representing the Alpha Macro
    Int : A type representing the Int Macro
    NewLineMacro : A type representing the New Line Macro
    Deleted : A type representing a deleted rule
    Tok ADT : A type representing something with the tok modifier
    Many ADT : A type representing something with the many modifier
    Some ADT : A type representing something with the some modifier
    Optional ADT : A type representing something with the optional modifier
-}

data ADT = Terminal String
    | NonTerminal String
    | Parameter String [ADT]
    | Alpha
    | Int
    | NewLineMacro
    | Deleted
    | Tok ADT
    | Many ADT
    | Some ADT
    | Optional ADT
    | Consecutive ADT ADT
    | Alternate ADT ADT
    | OneLine ADT ADT
    | EqualSign ADT ADT
    | NormalParam String
    deriving (Show, Eq)

nonTerminal :: Parser ADT
nonTerminal = do
    _ <- is '<'
    letter <- lower
    rest <- many alphaNumUnder
    _ <- is '>'
    pure (NonTerminal (letter : rest))

terminal :: Parser ADT
terminal = is '\"' >> Terminal <$> many (isNot '\"') <* is '\"'

alphaMacro :: Parser ADT
alphaMacro = checkMacros "alpha" Alpha

newLineMacro :: Parser ADT
newLineMacro = checkMacros "newline" NewLineMacro

intMacro :: Parser ADT
intMacro = checkMacros "int" Int

macros :: Parser ADT
macros = alphaMacro <|> intMacro <|> newLineMacro

normalParam :: Parser ADT
normalParam = NormalParam <$> some alpha

-- | Parses a parameter on the RHS of the equal sign if the parameters are
-- not defined yet in the LHS
parameterRule :: Parser ADT
parameterRule = do
    _ <- is '<'
    letter <- lower
    rest <- many alphaNumUnder
    _ <- charTok '('
    
    -- Reads inside the bracket for atleast one parameter inside
    -- commaTok <* inlineSpaces to delete all white space between the commas
    params <-  some (asum [sepBy normalParam (commaTok <* inlineSpaces), sepBy modifiers (commaTok <* inlineSpaces), 
                           sepBy nonTerminal (commaTok <* inlineSpaces), sepBy macros (commaTok <* inlineSpaces), 
                           sepBy terminal (commaTok <* inlineSpaces), sepBy parameterRule (commaTok <* inlineSpaces)])
    _ <- is ')'
    _ <- is '>'
    pure (Parameter (letter:rest) params)

-- | Parses a parameter on the LHS or on the RHS of the equal if the parameters are
-- already defined in the LHS
parameterRuleFac :: Parser ADT
parameterRuleFac = do
    _ <- is '<'
    letter <- lower
    rest <- many alphaNumUnder
    _ <- charTok '('
    
    -- Reads inside the bracket for atleast one parameter inside
    -- commaTok <* inlineSpaces to delete all white space between the commas
    params <-  some (asum [sepBy normalParam (commaTok <* inlineSpaces), sepBy modifiers (commaTok <* inlineSpaces), 
                           sepBy nonTerminal (commaTok <* inlineSpaces), sepBy macros (commaTok <* inlineSpaces), 
                           sepBy terminal (commaTok <* inlineSpaces), sepBy paramRef (commaTok <* inlineSpaces), 
                           sepBy parameterRuleFac (commaTok <* inlineSpaces)])
    _ <- is ')'
    _ <- is '>'
    pure (Parameter (letter:rest) params)

-- | Parses parameter references from the string
paramRef :: Parser ADT
paramRef = is '[' >> NormalParam <$> many alphaNumUnder <* is ']'

-- | Parses tok Modifiers from the string, and depending if it has a parameter on the
-- LHS, then it will also consider in the parameter references too 
tokPattern :: Parser ADT
tokPattern = string "tok" >> inlineSpaces1 >> Tok <$> (nonTerminal <|> macros <|> terminal <|> parameterRule)
tokPatternParamRef :: Parser ADT
tokPatternParamRef = string "tok" >> inlineSpaces1 >> Tok <$> (nonTerminal <|> macros <|> terminal <|> parameterRuleFac <|> paramRef)

-- | Parses many Modifiers from the String, and depending if it has a parameter on the
-- LHS, then it will also consider in the parameter references too 
manyPattern :: Parser ADT
manyPattern = symbolPattern '*' Many
manyPatternParam:: Parser ADT
manyPatternParam = symbolPatternFac '*' Many

-- | Parses some Patter from the String, and depending if it has a parameter on the
-- LHS, then it will also consider in the parameter references too 
somePattern :: Parser ADT
somePattern = symbolPattern '+' Some
somePatternParam :: Parser ADT
somePatternParam = symbolPatternFac '+' Some

-- | Parses optional Modifiers from the String, and depending if it has a parameter on the
-- LHS, then it will also consider in the parameter references too 
optionalPattern :: Parser ADT
optionalPattern = symbolPattern '?' Optional
optionalPatternParam :: Parser ADT
optionalPatternParam = symbolPatternFac '?' Optional

-- | Parses any Modifiers from the string, and if it has a parameter on the LHS
-- then it will also consider in the parameter references too
modifiers :: Parser ADT
modifiers = manyPattern <|> optionalPattern <|> tokPattern <|> somePattern
modifiersParam :: Parser ADT
modifiersParam = manyPatternParam <|> optionalPatternParam <|> tokPatternParamRef <|> somePatternParam

-- | Parses an equal sign to connect the LHS and RHS of the bnf rule
equal :: Parser (ADT -> ADT -> ADT)
equal = inlineSpaces1 >> string "::=" >> inlineSpaces1 >> pure EqualSign

-- | Parses an alternate to connect the rules between alternates
alternate :: Parser (ADT -> ADT -> ADT)
alternate = inlineSpaces1 >> is '|' >> inlineSpaces1 >> pure Alternate

-- | Parses consecutive bnfValues together
aRule :: Parser (ADT -> ADT -> ADT)
aRule = inlineSpaces1 >> pure Consecutive

-- | Parses a single line of a bnf rule to connect lines of rules together
oneLine :: Parser (ADT -> ADT -> ADT)
oneLine = inlineSpaces >> charTok '\n' >> pure OneLine

-- | Parses any bnfValues, and depending if the LHS had a defined
-- parameter, then it will also consider the parameter references on the RHS
bnfValues :: Parser ADT
bnfValues = modifiers <|> nonTerminal <|> macros <|> terminal
bnfValuesParam :: Parser ADT
bnfValuesParam = modifiersParam <|> nonTerminal <|> macros <|> terminal

-- | Parses consecutive bnfValues together, and depending if the LHS had a defined
-- parameter, then it will also consider the parameter references on the RHS
bnfConsecutive:: Parser ADT
bnfConsecutive = lowChain (parameterRule <|> bnfValues) aRule
bnfConsecutiveParam :: Parser ADT
bnfConsecutiveParam = lowChain (parameterRuleFac <|> bnfValuesParam <|> paramRef) aRule

-- | Parses alternatives together, and depending if the LHS had a defined
-- parameter, then it will also consider the parameter references on the RHS
bnfRule :: Parser ADT
bnfRule = lowChain bnfConsecutive alternate
bnfRuleParam :: Parser ADT
bnfRuleParam = lowChain bnfConsecutiveParam alternate

-- | Parses an equal sign to connect the LHS and RHS of the bnf rule, and checks if the
-- RHS of the bnf is a parameter or not
bnfLine :: Parser ADT
bnfLine = (equalChain bnfRuleParam equal <* inlineSpaces) <|> (equalChain bnfRule equal <* inlineSpaces)

-- | Parses a Input bnf and goes through it line-by-line and then connects each line
-- together, resulting in the final ADT
bnfParser :: Parser ADT
bnfParser = spaces >> topChain bnfLine oneLine


--- Generate to Haskell Code ---
adtToString :: ADT -> String
adtToString inp = case inp of
    Consecutive x y -> adtToString x ++ " " ++ adtToString y
    NonTerminal name -> upperCaseLetter name
    Terminal _ -> "String"
    Int -> "Int"
    Alpha -> "String"
    NewLineMacro -> "Char"
    Tok x -> adtToString x
    NormalParam x -> x
    Parameter name param -> "("  ++ upperCaseLetter name ++ " " ++ init spaced  ++")"
        where
            spaced = foldl (\x b -> adtToString b ++ " " ++ x) "" param
    Many x -> "[" ++ adtToString x ++ "]"
    Some x -> "Some" ++ adtToString x
    Optional x -> "(Maybe " ++ adtToString x ++ ")"
    _ -> "error"

-- | Converts each alternate rule into its own type in string, 
-- starting from the last-most rule and concatenates them together
addAlternates :: String -> Int -> Int -> ADT -> String
addAlternates name spacelen curlevel (Alternate x y) = addAlternates name spacelen (curlevel-1) x
    ++ replicate spacelen ' '
    ++ "| "
    ++ name
    ++ show curlevel
    ++ " "
    ++ adtToString y
    ++ "\n"
addAlternates name _ a x = 
    name
    ++ show a
    ++ " "
    ++ adtToString x
    ++ "\n"

-- | Checks if the RHS has an alternate or has more than a single field
-- then creates the type accordingly
typesAlternate :: String -> ADT -> String
typesAlternate name rule@(Alternate _ _) = 
    firstHalf
    ++ " "
    ++ addAlternates expr spaceLength depth rule
    ++ "    deriving Show\n\n"
    where
        expr = upperCaseLetter name
        firstHalf = "data " ++ expr ++ " ="
        spaceLength = length firstHalf -1

        -- Finds the depth to be used as numbers in creating 
        -- the different data types
        depth = numOfAlt rule 1
typesAlternate name rule@(Consecutive _ _) = 
    firstHalf
    ++ " "
    ++ upperCased
    ++ "1 "
    ++ adtToString rule
    ++ "\n"
    ++ "    deriving Show\n\n"
    where
        upperCased = upperCaseLetter name
        firstHalf = "data " ++ upperCased ++ " ="
typesAlternate name rule = 
    "newtype "
    ++ upperCased
    ++ " = "
    ++ upperCased
    ++ " "
    ++ adtToString rule
    ++ "\n"
    ++ "    deriving Show\n\n"
    where
        upperCased = upperCaseLetter name

-- | Checks if the RHS has an alternate or has more than a single field
-- then creates the type accordingly if the RHS was a parameter
typesAlternateParam :: String -> [ADT] -> ADT -> String
typesAlternateParam name param rule@(Alternate _ _) =
    firstHalf
    ++ " "
    ++ addAlternates expr spaceLength depth rule
    ++ "    deriving Show\n\n"
    where
        expr = upperCaseLetter name
        spaced = foldl (\x y -> x ++ " " ++ adtToString y) "" param
        firstHalf = "data " ++ expr ++ spaced ++ " ="
        spaceLength = length firstHalf -1

        -- Finds the depth to be used as numbers in creating 
        -- the different data types
        depth = numOfAlt rule 1
typesAlternateParam name param rule@(Consecutive _ _) = 
    firstHalf
    ++ " "
    ++ expr
    ++ "1 "
    ++ adtToString rule
    ++ "\n    deriving Show\n\n"
    where
        expr = upperCaseLetter name
        spaced = foldl (\x y -> x ++ " " ++ adtToString y) "" param
        firstHalf = "data " ++ expr ++ spaced ++ " ="
typesAlternateParam name param rule =
    firstHalf
    ++ " "
    ++ upperCased
    ++ "1 "
    ++  adtToString rule
    ++ "\n    deriving Show\n\n"
    where
        spaced = foldl (\x y -> x ++ " " ++ adtToString y) "" param
        upperCased = upperCaseLetter name
        expr = upperCaseLetter name
        firstHalf = "newtype " ++ expr ++ spaced ++ " ="

-- | Generate the string representation of the types for a bnf ADT
generateTypes :: ADT -> String
generateTypes (OneLine x a) = generateTypes x ++ generateTypes a
generateTypes (EqualSign (Parameter x param) y) = typesAlternateParam x param y
generateTypes (EqualSign (NonTerminal x) y) = typesAlternate x y
generateTypes Deleted = ""
generateTypes _ = "Error"

-- | Turns the adt into its correct string intepretation for its parser
parserADTToString :: ADT -> String
parserADTToString inp = case inp of
    Consecutive x y -> parserADTToString x ++ " <*> " ++ parserADTToString y
    NonTerminal x ->  x
    Terminal x -> "(string \"" ++ x ++ "\")"
    Int -> "int"
    Alpha -> "(some alpha)"
    NewLineMacro -> "(is \'\\n\')"
    NormalParam x -> x
    Tok x -> case x of
        Terminal y -> "(stringTok " ++ "\"" ++ y ++ "\"" ++ ")"
        rest -> "(tok " ++ parserADTToString rest ++ ")"
    Parameter name param -> "("++ name ++ " " ++ init spaced ++ ")"
        where
            spaced = foldl (\x a -> parserADTToString a ++ " " ++ x) "" param
    Many x -> "(many " ++ parserADTToString x ++ ")"
    Some x -> "(some " ++ parserADTToString x ++ ")"
    Optional x -> "(optional " ++ parserADTToString x ++ ")"
    _ -> "Error"

-- | Converts each alternate rule into its own parser in string, 
-- starting from the last-most rule and concatenates them together
alternaterParser :: String -> Int -> Int -> ADT -> String
alternaterParser name spacelen curlevel (Alternate x y) = alternaterParser name spacelen (curlevel-1) x
    ++ replicate spacelen ' '
    ++ "<|> "
    ++ name
    ++ show curlevel
    ++ " <$> "
    ++ parserADTToString y
    ++ "\n"
alternaterParser name _ curlevel rule =
    name
    ++ show curlevel
    ++ " <$> "
    ++ parserADTToString rule
    ++ "\n"

-- | Checks if the RHS has an alternate or has more than a single field
-- then creates the parser accordingly, and the LHS is not a parameter
parserToString :: String -> ADT -> String
parserToString name rule@(Alternate _ _) = 
    name
    ++ " :: "
    ++ parserRule name []
    ++ "\n"
    ++ fsthalf
    ++ alternaterParser upperCaseName spacelength depth rule
    ++ "\n"
    where
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ " = "
        spacelength = length fsthalf - 2

        -- Finds the depth to be used as numbers in creating 
        -- the different parsers
        depth = numOfAlt rule 1
parserToString name rule@(Consecutive _ _) =
    name
    ++ " :: "
    ++ parserRule name []
    ++ "\n"
    ++ fsthalf
    ++ upperCaseName
    ++ "1"
    ++ " <$> "
    ++ parserADTToString rule
    ++ "\n\n"
    where
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ " = "
parserToString name rule =
    name
    ++ " :: "
    ++ parserRule name []
    ++ "\n"
    ++ fsthalf
    ++ upperCaseName
    ++ " <$> "
    ++ parserADTToString rule
    ++ "\n\n"
    where
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ " = "

-- | Checks if the RHS has an alternate or has more than a single field
-- then creates the parser accordingly, and the LHS is a parameter
parserToStringParam :: String -> [ADT] -> ADT -> String
parserToStringParam name param rule@(Alternate _ _) = 
    name
    ++ " :: "
    ++ parserRule upperCaseName param
    ++ "\n"
    ++ fsthalf
    ++ alternaterParser upperCaseName spacelength depth rule
    ++ "\n"
    where
        spaced = foldl (\x y -> x ++ " " ++ parserADTToString y) "" param
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ spaced ++ " ="
        spacelength = length fsthalf - length spaced - 1

        -- Finds the depth to be used as numbers in creating 
        -- the different data parsers
        depth = numOfAlt rule 1
parserToStringParam name param rule@(Consecutive _ _) =
    name
    ++ " :: "
    ++ parserRule upperCaseName param
    ++ "\n"
    ++ fsthalf
    ++ upperCaseName
    ++ "1"
    ++ " <$> "
    ++ parserADTToString rule
    ++ "\n\n"
    where
        spaced = foldl (\x y -> x ++ " " ++ parserADTToString y) "" param
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ spaced ++ " = "
parserToStringParam name param rule =
    name
    ++ " :: "
    ++ parserRule upperCaseName param
    ++ "\n"
    ++ fsthalf
    ++ upperCaseName
    ++ " <$> "
    ++ parserADTToString rule
    ++ "\n\n"
    where
        spaced = foldl (\x y -> x ++ " " ++ parserADTToString y) "" param
        upperCaseName = upperCaseLetter name
        fsthalf = name ++ spaced ++ " = "

-- | Generate the string representation of the parsers for a bnf ADT
generateParser :: ADT -> String
generateParser (OneLine x a) = generateParser x ++ generateParser a
generateParser (EqualSign (NonTerminal x) y) = parserToString x y
generateParser (EqualSign (Parameter x param) y) = parserToStringParam x param y
generateParser Deleted = ""
generateParser _ = "Error" -- Set as Error to Debug

-- | Validates the bnf rule by checking if there are any duplicates, undefined, 
-- or left reecursive rules, and any deleted rules will result in it stopping and
-- returning the current bnf instantly with the rule that has issues being marked as deleted
validatedRule :: ADT -> ADT -> ADT
validatedRule (OneLine a r@(EqualSign _ _)) accum = 
    case (checkDupe r accum, checkUndef r accum, checLeftRecurs r accum) of
    (Deleted, _, _) -> OneLine a Deleted
    (_, Deleted, _) -> OneLine a Deleted
    (_, _, Deleted) -> OneLine a Deleted
    _ -> OneLine (validatedRule a accum) r
validatedRule r@(EqualSign _ _) accum = 
    case (checkDupe r accum, checkUndef r accum, checLeftRecurs r accum) of
    (Deleted, _, _) -> Deleted
    (_, Deleted, _) -> Deleted
    (_, _, Deleted) -> Deleted
    _ -> r -- Returns the bnf if there is no deleted rule
validatedRule (OneLine a Deleted) accum = OneLine (validatedRule a accum) Deleted
validatedRule x _ = x -- Returns the bnf if there is no deleted rule

-- | Validates the input BNF, and will keep recursively calling it self
-- until it does not see anymore rules being deleted. This ensures that the validation
-- is done iteratively
validatedBNF :: ADT -> Int -> ADT
validatedBNF bnf deletecount = if curdelete > deletecount
        then validatedBNF res curdelete
        else res
    where
        res = validatedRule bnf bnf
        curdelete = checkDeletes res 0

generateHaskellCode :: ADT -> String
generateHaskellCode bnf =  init (generateTypes validated ++ generateParser validated)
    where
        validated = validatedBNF bnf 0


--- Validation ---
-- | Goes through each rule and checks in each of them any violations, and
-- seenDupes is used for detecting duplicates (The same duplicate rule
-- won't be counted as a duplicate twice)
checkRule :: ADT -> ADT -> [ADT] -> [String]
checkRule (OneLine a (EqualSign lhs line)) bnf seenDupes = leftRecur :
    undef : 
    duplicate : 
    checkRule a bnf (lhs : seenDupes)
    where
        leftRecur = checkLeftRecursTraverse lhs bnf bnf (getLeftMostElem line) [lhs]
        undef = checkUndefined lhs bnf (getAdtfromRule line)
        duplicate = case checkDuplicate lhs bnf False of
            "" -> ""
            result
                | length (foldl (\x y -> if extractName y == extractName lhs then y:x else x) [] seenDupes) /= 1 -> ""
                | otherwise -> result
checkRule (EqualSign lhs line) bnf seenDupes = leftRecur :
    duplicate : 
    [undef]
    where
        leftRecur = checkLeftRecursTraverse lhs bnf bnf (getLeftMostElem line) [lhs]
        undef = checkUndefined lhs bnf (getAdtfromRule line)
        duplicate = case checkDuplicate lhs bnf False of
            "" -> ""
            result
                | length (foldl (\x y -> if extractName y == extractName lhs then y:x else x) [] seenDupes) /= 1 -> ""
                | otherwise -> result
checkRule _ _  _= ["Error"]

validate :: ADT -> [String]
validate bnf = reverse (filter (/= "") (checkRule bnf bnf []))

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime




----- Functions -----

-- Chain function From Course Notes & Moodle
-- FIT2102 Teaching Team. (n.d.-b). Unit: FIT2102 Programming paradigms - S2 2025, 
-- Section: Parser Combinators in Haskell | MonashELMS1. Moodle. https://learning.monash.edu/course/view.php?id=31365§ion=48

-- | The low chain works by parsing values and then connecting it using an operator
-- If the operator fails then it will return what it has currently using pure a
lowChain :: Parser ADT -> Parser (ADT -> ADT -> ADT) -> Parser ADT
lowChain p op = p >>= rest
    where
        rest :: ADT -> Parser ADT
        rest a = do
                    f <- op
                    b <- p
                    rest (f a b)
                <|> pure a

-- | The equal Chain works by parsing a line and then connecting it using
-- EqualSign, and that it checks if the LHS is a parameter non terminal
-- If the LHS is a parameterized non terminal, then it will go to a specialized
-- loop where it can check the rule for any parameter references and reject for
-- any references to a parameter that is not defined in the LHS
equalChain :: Parser ADT -> Parser (ADT -> ADT -> ADT) -> Parser ADT
equalChain p op = p >>= rest
    where
        rest2 :: ADT -> [ADT] -> Parser ADT
        rest2 a param = do
            f <- op
            b <- p
            if checkParam b param
                    then rest2 (f a b) param <|> pure a
                    else failed (UnexpectedString "") --Forces to fail
            <|> pure a
        
        rest :: ADT -> Parser ADT
        rest a = case a of
            (Parameter _ param) -> do
                f <- op
                b <- p
                if checkParam b param
                    then rest2 (f a b) param <|> pure a
                    else failed (UnexpectedString "") --Forces to fail
            _ -> do
                    f <- op
                    b <- p
                    if checkParam b []
                    then rest (f a b) <|> pure a
                    else failed (UnexpectedString "") --Forces to fail
                <|> pure a


-- | The top Chain works by parsing the multi-line input and then connecting
-- each line of the adt using the OneLine ADT, and then it checks if everyline
-- has an equal sign as the start and if not then it will fail by throwing an error
-- It also checks if the parse was successful if there are still things yet to be parsed
topChain :: Parser ADT -> Parser (ADT -> ADT -> ADT) -> Parser ADT
topChain p op = inlineSpaces >> p >>= rest
    where
        rest :: ADT -> Parser ADT
        rest a = (
            do
                f <- op
                b <- p
                if checkEqual b --Ensures that everyline always starts with the nonTerm ::= rule format
                    then rest (f a b)
                    else do
                        _ <- eof
                        failed (UnexpectedString "")   --Force it to throw an error
                ) <|> (
                    do
                        _ <- spaces --Deletes all trailing whitespace
                        eof -- Checks for anything unparsed input
                        pure a
                )

-- ## Parser Helper Functions
upperCaseLetter :: String -> String
upperCaseLetter (fs:rest) = toUpper fs : rest
upperCaseLetter [] = ""

alphaNumUnder :: Parser Char
alphaNumUnder = alpha <|> digit <|> is '_'

checkMacros :: String -> ADT-> Parser ADT
checkMacros str adt = is '[' >> adt <$ string str <* is ']'

-- | A parser to get the first parser name based on the generated Haskell Code
getFirstParser :: Parser String
getFirstParser = do
    _ <- stringTok "data" <|> stringTok "newtype"
    many (isNot ' ')
parseFirstParser :: String -> String
parseFirstParser code = case parse getFirstParser code of
    Result _ res -> res
    _ -> "Error"

checkEqual :: ADT -> Bool
checkEqual (EqualSign _ _) = True
checkEqual _ = False

-- | Checks if its a parameter and if the parameter references are inside
-- of the parameter that's defined in the LHS
checkParam :: ADT -> [ADT] -> Bool
checkParam (Alternate a b) l = checkParam a l && checkParam b l
checkParam (Consecutive a b) l = checkParam b l && checkParam a l --Forces a short circuit
checkParam (Parameter _ b) l = foldl (\x y -> checkParam y l && x) True b
checkParam r@(NormalParam _) l = r `elem` l
checkParam x l = case x of
    (Some y) -> checkParam y l
    (Many y) -> checkParam y l
    (Optional y) -> checkParam y l
    (Tok y) -> checkParam y l
    _ -> True

symbolPattern :: Char -> (ADT -> ADT) -> Parser ADT
symbolPattern c adt= do
    bnfParsed <- nonTerminal <|> macros <|> terminal <|> parameterRule
    _ <- is c
    pure (adt bnfParsed)

symbolPatternFac :: Char -> (ADT -> ADT) -> Parser ADT
symbolPatternFac c adt= do
    bnfParsed <- nonTerminal <|> macros <|> terminal <|> parameterRuleFac <|> paramRef
    _ <- is c
    pure (adt bnfParsed)

-- ## Generate Haskell Code Helper Functions

-- | Creates the type signature of the parser
parserRule :: String -> [ADT] -> String
parserRule name [] = "Parser " ++ upperCaseLetter name
parserRule name param = parsedrule ++ "Parser (" ++ name ++ spaced ++ ")"
    where
        parsedrule = foldl (\x y -> x ++ "Parser " ++ parserADTToString y ++ " -> ") "" param
        spaced = foldl (\x y -> x ++ " " ++ parserADTToString y) "" param

-- | Checks the number of deleted rules in the ADT
checkDeletes :: ADT -> Int -> Int
checkDeletes (OneLine a Deleted) accum = checkDeletes a (accum + 1)
checkDeletes (OneLine a _) accum = checkDeletes a accum
checkDeletes Deleted accum = accum + 1
checkDeletes _ accum = accum

numOfAlt :: ADT -> Int -> Int
numOfAlt (Alternate y _) x = numOfAlt y x+1
numOfAlt _ x = x

-- | Uses the output of checkLeftRecursTraverse to determine
-- if there is a left recursion or not
checLeftRecurs :: ADT -> ADT -> ADT
checLeftRecurs rule@(EqualSign b c) bnf 
    | res == "" = rule
    | otherwise = Deleted
    where 
        res = checkLeftRecursTraverse b bnf bnf (getLeftMostElem c) [b]
checLeftRecurs _ _ = Deleted

-- | Uses the output of checkUndefined to determine
-- if there are any undefined non temrinals or not
checkUndef :: ADT -> ADT -> ADT
checkUndef rule@(EqualSign b c) bnf
    | res == "" = rule
    | otherwise = Deleted
    where 
        res = checkUndefined b bnf (getAdtfromRule c)
checkUndef _ _ = Deleted

-- | Uses the output of checkDuplicate to determine
-- if there are any duplicates or not
checkDupe :: ADT -> ADT -> ADT
checkDupe rule@(EqualSign b _) bnf
    | res == "" = rule
    | otherwise = Deleted
    where 
        res = checkDuplicate b bnf False
checkDupe _ _ = Deleted

-- ## Validation Helper Functions
extractName :: ADT -> String
extractName inp = case inp of
    NonTerminal x ->  x
    NormalParam x -> x
    Parameter name _ -> name
    _ -> ""

checkSimilarity :: ADT -> ADT -> Bool
checkSimilarity a b = extractName a == extractName b

getAdtfromRule :: ADT -> [ADT]
getAdtfromRule (Alternate a b) = getAdtfromRule a ++ getAdtfromRule b
getAdtfromRule (Consecutive a adt) = case adt of
        (NonTerminal _) -> adt : getAdtfromRule a
        (Parameter _ _) -> adt : getAdtfromRule a
        _ -> getAdtfromRule a
getAdtfromRule a = case a of
        (NonTerminal _) -> [a]
        (Parameter _ _) -> [a]
        _ -> []

-- | Gets the left-most element in a rule
getLeftMostElem :: ADT -> [ADT]
getLeftMostElem (Alternate a b) = getLeftMostElem a ++ getLeftMostElem b
getLeftMostElem (Consecutive a _) = getLeftMostElem a
getLeftMostElem a = case a of
        (NonTerminal _) -> [a]
        (Parameter _ _) -> [a]
        _ -> []

-- | Checks for any undefined non terminals in a rule by filtering the
-- non terminals which are defined and if there still exists some non terminals
-- in the list then those non terminals are not defined in the bnf
checkUndefined :: ADT -> ADT -> [ADT] -> String
checkUndefined lhs (OneLine a (EqualSign cur _)) seenInRule = checkUndefined lhs a (filter (\x -> not (checkSimilarity x cur)) seenInRule)
checkUndefined lhs (OneLine a _) seenInRule = checkUndefined lhs a seenInRule
checkUndefined lhs (EqualSign cur _) seenInRule
    | not (null finalarr) = extractName lhs
    | otherwise = ""
    where
        lastarr = filter (\x -> not (checkSimilarity x cur)) seenInRule
        finalarr = filter (\x -> not (checkSimilarity x lhs)) lastarr
checkUndefined _ _ _ = ""

-- | Checks for any duplicates by ensuring that it only outputs the error when it 
-- sees the same non terminal more than once and that it has not been seen before,
-- this is to prevent it from reporting the same duplicated rule more than once
checkDuplicate :: ADT -> ADT -> Bool -> String
checkDuplicate lhs (OneLine a (EqualSign cur _)) seen
    | checkSimilarity cur lhs && seen = "Duplicate Rule: " ++ extractName cur
    | checkSimilarity cur lhs && not seen = checkDuplicate lhs a (not seen)
    | otherwise = checkDuplicate lhs a seen
checkDuplicate lhs (OneLine a _) seen = checkDuplicate lhs a seen
checkDuplicate lhs (EqualSign cur _) seen
    | checkSimilarity cur lhs && seen = "Duplicate Rule: " ++ extractName cur
    | otherwise = ""
checkDuplicate _ _ _ = ""

-- | Checks for left recursion by iteratively looping through the bnf with every newly
-- added left-most non terminal into the list, and if the LHS non temrinal is inside
-- the list then there is a infact a left recursion. It stops from an infinite recursio
-- by only adding the left-most nonterminals if the rule hasn't been checked
checkLeftRecursTraverse :: ADT -> ADT -> ADT -> [ADT] -> [ADT] -> String
checkLeftRecursTraverse lhs (OneLine a (EqualSign b c)) bnf seen checked = if b `elem` seen && (b `notElem` checked)
                                    then checkLeftRecursTraverse lhs bnf bnf (seen ++ getLeftMostElem c) (b : checked)
                                    else checkLeftRecursTraverse lhs a bnf seen checked
checkLeftRecursTraverse lhs (OneLine a _) bnf seen checked = checkLeftRecursTraverse lhs a bnf seen checked
checkLeftRecursTraverse lhs (EqualSign a b) bnf seen checked = if a `elem` seen && (a `notElem` checked)
                                    then checkLeftRecursTraverse lhs bnf bnf (seen ++ getLeftMostElem b) (a : checked)
                                    else checkLeftRecursTraverse lhs a bnf seen checked
checkLeftRecursTraverse lhs _ _ seen _ = if res then "Left Recursion in: " ++ extractName lhs else ""
    where
        res = foldl (\x y -> checkSimilarity y lhs || x) False seen