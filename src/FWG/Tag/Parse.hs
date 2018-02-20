
module FWG.Tag.Parse where

import FWG.Tag



import Text.Parsec
import Text.Parsec.Text


import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T

import System.IO


getTagsFromFile :: FilePath -> IO (Either ParseError [Tag])
getTagsFromFile filename = (readFile filename) >>= return . parse tags filename
--		w <- readFile filename
--		return $ parse tags filename w

-- parsing functions follow

tx = T.pack

line :: Parsec String () String
h3 :: Parsec String () ()
h2 :: Parsec String () ()
h1 :: Parsec String () String

h3 =  string "***" >> spaces
h2 = string "**" >> spaces
h1 = string "*"

tags = do
     ts <- many tag
     return ts

tag = do
    spaces
    name <- tagHeader
    (reqs, cs) <- tagBody
    spaces
    return $ Tag name reqs cs

tagHeader = do
	  h1
	  realSpaces
	  name <- manyTill anyChar (char '\n')
	  return $ tx name

tagBody = do
	reqs <- requirements
	spaces
	cs <- contents
	return (reqs, cs)



requirements = do
	     h2
	     string "requires\n"
	     reqs <- many requirementSpec
	     return reqs


requirementSpec = do
		req <- try reqAll <|> reqAny <|> reqNoneOf
		newline
		return req

reqAll = do
       string "all:"
       reqs <- fields
       return $ All $ tx <$> reqs

reqAny = do
       string "any:"
       reqs <- fields
       return $ Any $ tx <$> reqs


test1 :: Parsec String () String
test1 = do
      x <- many letter
      return x

reqNoneOf = do
	  string "not:"
	  reqs <- fields
	  return $ NoneOf $ tx <$> reqs



realSpaces = do
	   x <- many (string " ")
	   return x

fields = do
       realSpaces
       ws <- sepBy (many letter) sep
       return ws

sep :: Parsec String () [String]
sep = realSpaces >> (char ',') >> realSpaces
--sep = do
--    realSpaces
--    char ','
--    realSpaces


contents = do
	 h2
	 string "contents\n"
	 cs <- many $ try contentBlock 
	 return cs

contentBlock = do
	     h3
	     c <- try (description <|> places <|> try creatures <|> complications <|> things <|> adventures)
	     return c

description = do
	    string "description\n"
	    w <- line
	    return $ Description $ tx w

places = do
       string "places\n"
       ws <- many line 
       return $ Places $ tx <$> ws

creatures = do
	  string "creatures\n"
	  ws <- many line
	  return $ Creatures $ tx <$> ws

complications = do
	      string "complications\n"
	      ws <- many line
	      return $ Complications $ tx <$> ws

things = do
      string "things\n"
      ws <- many line
      return $ Things $ tx <$> ws

adventures = do
	   string "adventures\n"
	   ws <- many line
	   return $ Adventures $ tx <$> ws


line = manyTill (noneOf "\n*") newline
