module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language
import qualified Data.Map as Map

data XML = XMLElement {
    name :: String,
    attributes :: Map.Map String String,
    children :: [XML]
} | XMLText String
    deriving Show


-- Parse an XML node
nodeParser :: GenParser Char st XML
nodeParser = do
    (name, attributes) <- tagParser
    children <- bodyParser
    char '<' *> char '/' *> string name *> spaces *> char '>'
    return XMLElement {
        name=name,
        attributes=Map.fromList attributes,
        children= filter (\child -> case child of {(XMLText "") -> False; _ -> True}) children
    }


-- Parse the body of an XML node
bodyParser :: GenParser Char st [XML]
bodyParser = do
    text <- option "" textParser
    mnode <- optionMaybe (try nodeParser)
    case mnode of
        Nothing -> return [XMLText text]
        Just node -> do
            remaining <- bodyParser
            return $ XMLText text : node : remaining


-- Parse the text between two XML tags, either both opening or corresponinf opening and closing.
textParser :: GenParser Char st String
textParser = do
    first <- fmap return unescaped <|> escaped
    second <- textParser <|> return ""
    return $ first ++ second

escaped :: GenParser Char st String
escaped = do
    s <- char '\\'
    c <- anyChar
    return [s, c]

unescaped :: GenParser Char st Char
unescaped = noneOf "\\<>"


-- Parse the tag of an XML node
tagParser :: GenParser Char st (String, [(String, String)])
tagParser = do
    char '<'
    name <- nameParser
    spaces
    attributes <- attributeListParser
    spaces *> char '>'
    return (name, attributes)


-- Parse the attributes of an XML tag
attributeListParser :: GenParser Char st [(String, String)]
attributeListParser = try (spaces *> attributeParser) <|> return []

attributeParser :: GenParser Char st [(String, String)]
attributeParser = do
    key <- nameParser
    spaces *> char '=' *> spaces
    value <- stringLiteral $ makeTokenParser haskellDef
    others <- attributeListParser
    return $ (key, value) : others


-- Parse a valid XML name
nameParser :: GenParser Char st String
nameParser = do
    nameStart <- letter <|> oneOf ":_"
    rest <- many (letter <|> oneOf ":_-.")
    return $ nameStart : rest


main :: IO ()
main = do
    print "hi"
    print $ parse attributeListParser "" "hello =\"here\"there=\"now\">"
    print $ parse tagParser "" "<thisisatag hello =\"here\"  there=  \"now\"   >"
    print $ parse tagParser "" "<b>"
    print $ parse textParser "" "hello there<b>"
    print $ parse nodeParser "" "<b><c></c></b>"
    print $ parse nodeParser "" "<thisisatag hello =\"here\"  there=  \"now\"   >How's it going? <b>very well </b> that's good </thisisatag>"