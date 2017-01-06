import Data.List
import Data.Char
import Control.Monad
import Data.Maybe

data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person = Person {
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  mother   :: Maybe Person,
  father   :: Maybe Person,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)

pero  = Person "Pero" "Perić" Male    (Just ana) (Just ivo) Nothing    []
ana   = Person "Ana"  "Anić"  Female  (Just tea) (Just pero) Nothing    [pero]
tea   = Person "Tea"  "Teić"  Female  Nothing    Nothing (Just ivo) [ana]
ivo   = Person "Ivo"  "Ivić"  Male    Nothing    Nothing (Just tea) [ana]


-- INJECT se u monadama zove RETURN
-- BIND se zove >>=

--EXERCISE 1 ===========================================

grandfathersPartnerForename :: Person -> Maybe String
grandfathersPartnerForename p = father p >>= father >>= partner >>= return . forename

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suf s = stripPrefix (reverse suf) (reverse s) >>= return . reverse

removeAffixes :: String -> String -> String -> Maybe String
removeAffixes pre suf s = stripPrefix pre s >>= stripSuffix suf

-- EXERCISE 2 ===================================

grandfathersPartnerForename' :: Person -> Maybe String
grandfathersPartnerForename' p = do
  f <- father p
  g <- father f
  pr <- partner g
  return $ forename pr
