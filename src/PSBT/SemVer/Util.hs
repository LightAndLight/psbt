module PSBT.SemVer.Util (
    positiveDigit
) where

import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.String (Parser)

positiveDigit :: Parser Char
positiveDigit = char '1'
            <|> char '2'
            <|> char '3' 
            <|> char '4' 
            <|> char '5' 
            <|> char '6' 
            <|> char '7' 
            <|> char '8' 
            <|> char '9' 

