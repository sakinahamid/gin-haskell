-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Parser.Instances
import Data.Either
import Data.List
import Rummy.Play
import Rummy.Rules



instance Show Suit where
    show Diamond = "D"
    show Club = "C"
    show Heart = "H"
    show Spade = "S"

instance Show Rank where
    show Ace = "A"
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"

instance Show Card where
    show (Card s r) = show s ++ show r

instance Show Draw where
    show Stock = "ST"
    show Discard = "DI"

instance Show Meld where
    show (Straight5 a b c d e) = "ST5 " ++ show a ++ show b ++ show c ++ show d ++ show e
    show (Straight4 a b c d) = "ST4 " ++ show a ++ show b ++ show c ++ show d
    show (Straight3 a b c) = "ST3 " ++ show a ++ show b ++ show c
    show (Set4 a b c d) = "SE4 " ++ show a ++ show b ++ show c ++ show d
    show (Set3 a b c) = "SE3 " ++ show a ++ show b ++ show c
    show (Deadwood a) = "DW " ++ show a



--- General helper functions ---


-- Extract the value of a 'Just' but return a fallback in case of 'Nothing'.
-- Retrieved from FIT2102 Week 7 Tutorial (Maybes.hs)
getMaybe :: Maybe a -> a -> a
getMaybe (Just m) _ = m
getMaybe Nothing m = m

-- Retrieves stored value from a Maybe card, return "NC" to indicate Nothing
getMaybeCard :: Maybe Card -> String
getMaybeCard (Just x) = show x
getMaybeCard Nothing = "NC"

-- Retrieves stored value from a Maybe draw, return "ND" to indicate Nothing
getMaybeDraw :: Maybe Draw -> String
getMaybeDraw (Just x) = show x
getMaybeDraw Nothing = "ND"


-- Flatten a combined structure to a single structure.
-- join [[1, 2, 3], [1, 2]]
-- [1,2,3,1,2]
--
-- join (Just Nothing)
-- Nothing
--
-- join (Just (Just 7))
-- Just 7
join :: Monad f => f (f a) -> f a
join a = (=<<) (\x -> x) a

-- Apply a binary function in the environment.
-- Retrieved from FIT2102 Week 9 Tutorial (Exercises.hs)
--
-- lift (+) (Id 7) (Id 8)
-- Id 15
--
-- lift (+) [1, 2, 3] [4, 5]
-- [5,6,6,7,7,8]
--
-- lift (+) (Just 7) (Just 8)
-- Just 15
--
-- lift (+) (Just 7) Nothing
-- Nothing
--
-- lift (+) Nothing (Just 8)
-- Nothing
lift :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift = ((<*>) .) . (<$>)

-- Turn a list of structured items into a structured list of item, bailing on nils.
-- Retrieved from FIT2102 Week 9 Tutorial (Exercises.hs)
--
-- transform [Id 7, Id 8, Id 9]
-- Id [7,8,9]
--
-- transform [Just 7, Nothing]
-- Nothing
--
-- transform [Just 7, Just 8]
-- Just [7,8]
transform :: Applicative f => [f a] -> f [a]
transform = foldr (lift (:)) (pure []) 

-- Checks if any element in a list (list a) exists in another list (list b)
-- Rerieved from: https://stackoverflow.com/questions/27471710/checking-if-2-list-have-any-equal-element-haskell
exists :: (Eq a) => [a] -> [a] -> Bool
exists = flip (flip foldl False . flip . ((||) .) . flip elem)

-- Retrieved from https://tgdwyer.github.io/parsercombinators/ 
-- Gets memory out of parser
getMem :: ParseResult a -> a
getMem (Result _ cs) = cs
getMem (Error _) = error "An error has occured while parsing."

-- Count the scores of the rank of cards in a meld
meldScores :: [Meld] -> Int
meldScores = sum . (cardPoints <$>)

-- check the difference in scores when a card is included in a meld and when it is not
checkScores :: [Card] -> Card -> Int
checkScores xs x = meldScores inCard - meldScores exCard
    where
        inCard = makeMelds (0,0) "" (x:xs)
        exCard = makeMelds (0,0) "" xs

-- Retrieve card's rank
retRank :: Card -> Rank
retRank (Card _ r) = r

-- Retrieve card's suit
retSuit :: Card -> Suit
retSuit (Card s _) = s

-- Checks if all cards in the list have the same rank
checkRank :: [Card] -> Bool
checkRank [] = False
checkRank (y:ys) = same retRank y ys

-- Checks if all cards in the list have the same suit
checkSuit :: [Card] -> Bool
checkSuit [] = False
checkSuit (y:ys) = same retSuit y ys

-- Convert melds to cards
convertMeldedCards :: Meld -> [Card]
convertMeldedCards (Straight5 a b c d e) = [a, b, c, d, e]
convertMeldedCards (Straight4 a b c d) = [a, b, c, d]
convertMeldedCards (Straight3 a b c) = [a, b, c]
convertMeldedCards (Set4 a b c d) = [a, b, c, d]
convertMeldedCards (Set3 a b c) = [a, b, c]
convertMeldedCards (Deadwood a) = [a]

-- Sum the rank of a list of Cards
-- toPoints can convert card into points
totalRanks :: [Card] -> Int
totalRanks = sum . (toPoints <$>)

-- Sum up the scores of deadwoods given a meld
meldPointDeadwood :: [Meld] -> Int
meldPointDeadwood = sum . (cardPoints <$>)

-- Sum up the scores of deadwoods given a list of cards
calcScoreDeadwood :: [Card] -> Int
calcScoreDeadwood x = meldPointDeadwood $ makeMelds (0,0) "" x

scoreTest :: [Card] -> Card -> Int
scoreTest xs x = calcScoreDeadwood (x:xs) - calcScoreDeadwood xs

-- Retrieve deadwoods from a list of melds
retDeadwoods :: [Meld] -> [Card]
retDeadwoods cs = join $ convertMeldedCards <$> deadwoods
    where
        deadwoods = foldr (\a b -> if checkDeadwood a then a:b else b) [] cs

-- checks if input is a deadwood
checkDeadwood :: Meld -> Bool
checkDeadwood (Deadwood _) = True
checkDeadwood _ = False
 
-- -- Determine which pile to draw from
detPile :: [Card] -> Card -> String -> Bool
detPile xs x _
    -- case 1 = when including card from discard pile reduces/maintains value of deadwood, choose discard pile
    | score < 0 = True
    | score == 0 = True
    -- case 2 = when including card from discard pile increases value of deadwood but its rank is less than or equals to 5, choose discard pile
    | score > 0  && (toPoints x <= 5) = True
    | otherwise = False    -- choose stock pile
    where
        score = scoreTest xs x                        -- calculate deadwood score if dscarded card is included

-- Determine which pile to draw from (handles Nothing case for Maybe String)
cardPile :: [Card] -> Card -> Maybe String -> Bool
cardPile _ _ Nothing = False
cardPile xs x (Just m) = detPile xs x m

-- determine action based on a given hand and drawn card
detAction :: Card -> [Card] -> String 
detAction c cs
    | (length deadwoods) == 1  = "gin"
    | deadwoodCount <= 10 = "knock"
    | otherwise = "drop"
    where
        melds = makeMelds (0,0) "" (c:cs)
        deadwoods = retDeadwoods melds
        deadwoodCount = totalRanks deadwoods

-- compare melds based on rank in descending order
vsCards :: [Card] -> [Card] -> Ordering
vsCards = (. totalRanks) . compare . totalRanks

-- sort cards by rank in ascending order
rankBasedSort :: [[Card]] -> [[Card]]
rankBasedSort [] = []
rankBasedSort cs = sortBy vsCards cs 

-- sorts cards by rank in ascending order, then returns card with highest rank at the end of the list of cards
discardCard :: Card -> [Card] -> Card
discardCard dc cs = c
    where
        sortedHandList = head $ rankBasedSort [cs]
        sortedHandLast = last sortedHandList
        c = if (sortedHandLast == dc)
            then sortedHandList !! (length sortedHandList - 2)
        else sortedHandLast

-- sorts deadwoods by rank in ascending order, then returns card with highest rank at the end of the list of deadwoods
discardDeadwood :: Card -> [Card] -> Card
discardDeadwood dc cs = c
    where
        meldedCards = makeMelds (0,0) "" (dc:cs) -- separate melds and deadwoods
        deadwoods = retDeadwoods meldedCards -- retrieve list of deadwood(s)
        sortedDeadwoodsList = head $ rankBasedSort [deadwoods] -- sort list of deadwood(s) in ascending order based on rank
        sortedDeadwoodsLast = last sortedDeadwoodsList -- get the last element of the list; card with highest rank
        c = if (sortedDeadwoodsLast == dc && (length sortedDeadwoodsList == 1)) -- if there is only one deadwood and deadwood == card drawn
                then  discardCard dc (dc:cs)
            else if (sortedDeadwoodsLast == dc && (length sortedDeadwoodsList > 1)) -- if deadwood > 1 and deadwood == card drawn
                then sortedDeadwoodsList !! (length sortedDeadwoodsList - 2)
            else sortedDeadwoodsLast

-- Converts possible set(s) to meld
convertSet :: [Card] -> Maybe Meld
convertSet (a : b : c : d : _) = Just (Set4 a b c d)
convertSet (a : b : c : _) = Just (Set3 a b c)
convertSet _ = Nothing

-- Converts possible straight(s) to meld
convertStraight :: [Card] -> Maybe Meld
convertStraight (a : b : c : d : e : _) = Just (Straight5 a b c d e)
convertStraight (a : b : c : d : _) = Just (Straight4 a b c d)
convertStraight (a : b : c: _) = Just (Straight3 a b c)
convertStraight _ = Nothing

-- Converts deadwood(s) to meld
convertDeadwood :: [Card] -> Maybe Meld
convertDeadwood (a : _) = Just (Deadwood a)
convertDeadwood _ = Nothing

-- Remove duplicates
delDupl :: [[Card]] -> [Card] ->  [[Card]]
delDupl x y = foldr (\c cs -> if exists c (join cs) then cs else c:cs) [y] x

-- Retrieve all melds created
getMelds :: [[Card]] ->  [[Card]]
getMelds l = foldr (\x xs -> 
    if totalRanks (join(delDupl l x)) > totalRanks (join xs) 
    then (delDupl l x) 
    else xs) [[]] l



--- Implementation of parsing of memory ---
-- Implementation of parser and memory are heavily referenced from:
-- - https://tgdwyer.github.io/parsercombinators/
-- - https://repl.it/repls/FantasticQuirkyUnix 
-- - https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners


data MemoryData = MemoryData    
    { 
      totalDiscard :: Int -- total number of discarded cards
    , numStock :: Int -- total cards in stock pile
    , count :: Int -- to keep track of the game round
    , topCard :: Maybe Card -- current card on top of discard pile
    , drawPile :: Maybe Draw -- Player's stock/discard pile
    , drawPileOpp :: Maybe Draw -- Opponent's stock/discard pile
    }
    deriving (Show)

-- General constructor for MemoryData
constructMemoryData :: Int -> Int -> Int -> Maybe Card -> Maybe Draw -> Maybe Draw -> MemoryData
constructMemoryData a b c d e f = MemoryData {totalDiscard = a, numStock = b, count = c, topCard = d, drawPile = e, drawPileOpp = f}

-- Convert memory back to a string
convertMemData :: MemoryData -> String
convertMemData MemoryData {totalDiscard, numStock, count, topCard, drawPile, drawPileOpp} = show totalDiscard ++ " " ++ show numStock ++ " " ++ show count ++ " " ++ (getMaybeCard topCard) ++ " " ++ (getMaybeDraw drawPile) ++ " " ++ (getMaybeDraw drawPileOpp)

-- General parser for lists
-- From FIT2102 Week 11 Tutorial (Parser.hs)
list :: Parser a -> Parser [a]
list = (||| pure ([])) . list1
list1 :: Parser a -> Parser [a]
list1 = (<*>) (>>=) ((. ((pure .) . (:))) . (>>=) . list)

-- General parser for strings
string :: String -> Parser String
string = traverse is

-- General parser for whitespace
spaces :: Parser ()
spaces = (is ' ' >> spaces) ||| pure ()

-- Parser for one digit
oneDigit :: Parser Char
oneDigit = is '0' ||| is '1' ||| is '2' ||| is '3' ||| is '4' ||| is '5' ||| is '6' ||| is '7' ||| is '8' ||| is '9'

singleDigit :: Parser [Char]
singleDigit = do
    a <- oneDigit
    pure [a]

-- Parser for two digits
twoDigits :: Parser [Char]
twoDigits = do
    a <- oneDigit
    b <- oneDigit
    pure [a,b]

-- Parser for three digits
threeDigits :: Parser [Char]
threeDigits = do
    a <- oneDigit
    b <- oneDigit
    c <- oneDigit
    pure [a,b,c]

-- Parser for (up to 3) digits
digit :: Parser [Char]
digit = threeDigits ||| twoDigits ||| singleDigit
    
-- Parser for indiviual suits
diamond :: Parser Suit
diamond = is 'D' >> pure Diamond

club :: Parser Suit
club = is 'C' >> pure Club

heart :: Parser Suit
heart = is 'H' >> pure Heart

spade :: Parser Suit
spade = is 'S' >> pure Spade

-- General parser for suits
suit :: Parser Suit
suit = diamond ||| club ||| heart ||| spade

-- Parser for indiviual ranks
ace  :: Parser Rank
ace = string "A" >> pure Ace

two  :: Parser Rank
two =  string "2" >> pure Two

three :: Parser Rank
three = string "3" >> pure Three

four :: Parser Rank
four = string "4" >> pure Four

five :: Parser Rank
five = string "5" >> pure Five

six  :: Parser Rank
six = string "6" >> pure Six

seven :: Parser Rank
seven = string "7" >> pure Seven

eight :: Parser Rank
eight = string "8" >> pure Eight

nine :: Parser Rank
nine = string "9" >> pure Nine

ten  :: Parser Rank
ten = string "10" >> pure Ten

jack :: Parser Rank
jack = string "J" >> pure Jack

queen :: Parser Rank
queen = string "Q" >> pure Queen

king :: Parser Rank
king = string "K" >> pure King

-- General parser for ranks
rank :: Parser Rank
rank = ace ||| two ||| three ||| four ||| five ||| six ||| seven ||| eight ||| nine ||| ten ||| jack ||| queen ||| king

-- General parser for cards
oneCard :: Parser Card
oneCard = do
    cardSuit <- suit
    cardRank <- rank
    pure $ Card cardSuit cardRank

maybeOneCard :: Parser (Maybe Card)
maybeOneCard = do
    cardSuit <- suit
    cardRank <- rank
    pure $ Just $ Card cardSuit cardRank

maybeOneCardNull :: Parser (Maybe Card)
maybeOneCardNull = string "NC" >> pure Nothing

maybeCard :: Parser (Maybe Card)
maybeCard = maybeOneCard ||| maybeOneCardNull

cardList :: Parser [Card]
cardList = list oneCard

maybeCardList :: Parser [Maybe Card]
maybeCardList = list maybeCard

-- Parser for draw options
stock :: Parser Draw
stock = string "ST" >> pure Stock

discard :: Parser Draw
discard = string "DI" >> pure Discard

maybeStock :: Parser (Maybe Draw)
maybeStock = do
    s <- stock
    pure $ Just s

maybeDiscard :: Parser (Maybe Draw)
maybeDiscard = do
    d <- discard
    pure $ Just d

maybeDrawNull :: Parser (Maybe Draw)
maybeDrawNull = string "ND" >> pure Nothing

maybeDrawParser :: Parser (Maybe Draw)
maybeDrawParser = maybeStock ||| maybeDiscard ||| maybeDrawNull

--General parser for draw
drawParser :: Parser Draw
drawParser = stock ||| discard

readCard :: Card -> Maybe Card
readCard (Card s r) = Just (Card s r)

-- Main parser for memory
memoryData :: Parser MemoryData
memoryData = do
    td <- digit --totalDiscard
    spaces
    ns <- digit --numStock
    spaces
    c <- digit --count
    spaces
    sr <- maybeCard --topCard
    spaces
    dp <- maybeDrawParser --drawPile
    spaces
    dpo <- maybeDrawParser --drawPileOpp
    spaces
    return (MemoryData (read td :: Int) (read ns :: Int) (read c :: Int) sr dp dpo)

memoryDataNull :: Parser MemoryData
-- 1 = initial amount of discarded card
-- 31 = total cards in a deck - (total cards in a hand) * 2 - 1 card to start the discard pile
memoryDataNull = string "" >> return (MemoryData 1 31 0 Nothing Nothing Nothing)

parseMemory :: Parser MemoryData
parseMemory = memoryData ||| memoryDataNull

parseString :: String -> MemoryData
parseString m = getMem $ parse parseMemory m
        


--- AI implementation ---

-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
--
-- if cardPile is true, return discord
-- if cardPile is false, return stock
-- 
pickCard :: ActionFunc
pickCard c _ m d cs
    | cardPile cs c m == True    = (Discard, updateMem)
    | otherwise                  = (Stock, updateMem)
    where
        -- parse memory to retrieve info to update necessary data--
        td = totalDiscard $ parseString (getMaybe (m) (""))
        ns = numStock $ parseString (getMaybe (m) (""))
        co = count $ parseString (getMaybe (m) (""))

        -- update parts of memory to be updated into the main memory --
        updatedTotalDiscard = 
            if cardPile cs c m == True -- if drawPile = Discard
                then td - 1 
            else td
        updatedNumStock = 
            if  cardPile cs c m == False || getMaybeDraw d == show Stock -- if drawPile = Stock
                then ns - 1 
            else ns
        updatedCount = co + 1
        updatedTopCard = Just c
        updatedDrawPile = if cardPile cs c m == True then (Just Discard) else (Just Stock)
        updatedDrawPileOpp = d
        -- update the memory
        updateMem =  convertMemData $ constructMemoryData updatedTotalDiscard updatedNumStock updatedCount updatedTopCard updatedDrawPile updatedDrawPileOpp

-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard c _ m cs = (act, updatedMem)
    where
        detActionString = detAction c cs -- returns string of which action to take next

        -- to choose card to be discarded, there are two cases:
        -- (1) if there are more than 2 (knock/drop) separate melds from deadwoods, sort deadwoods based on its 
        --     rank in ascending order, discard the last element of the sorted deadwood if its not the card drawn, 
        --     or discard the second last element if the last element is the same as the card drawn.        
        -- (2) if there is only one deadwood (gin), and the deadwood is the same as the card drawn, sort the hand based on 
        --     rank and discard the last element of the sorted cards if its not the card drawn, or discard the 
        --     second last element if the last element is the same as the card drawn. May result in the loss of
        --     some meld(s).
        toDiscard = discardDeadwood c cs
        
        -- construct Action based on detActionString
        act = 
            if detActionString == "gin"
                then Action Gin toDiscard
            else if detActionString == "knock"
                then Action Knock toDiscard
            else Action Drop toDiscard
        
         -- parse memory to retrieve info to update necessary data--
        td = totalDiscard $ parseString m
        ns = numStock $ parseString m
        co = count $ parseString m
        tc = topCard $ parseString m
        dp = drawPile $ parseString m
        dpo = drawPileOpp $ parseString m

        -- update parts of memory to be updated into the main memory --
        updatedTotalDiscard = td + 1
        updatedNumStock = ns
        updatedCount = co
        updatedTopCard = tc
        updatedDrawPile = dp
        updatedDrawPileOpp = dpo
        -- update the memory
        updatedMem =  convertMemData $ constructMemoryData updatedTotalDiscard updatedNumStock updatedCount updatedTopCard updatedDrawPile updatedDrawPileOpp


-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
makeMelds :: MeldFunc
makeMelds _ _ l = getMaybe (transform melds) ([])
    where
        -- Checks if a list of cards (a hand) is a Set
        canSet :: [Card] -> Bool
        canSet = (<*>) ((&&) . (>= 3) . length) ((<*>) ((&&) . (<= 4) . length) checkRank)

        -- Checks if a list of cards (a hand) is a Straight (3/4/5)
        canStraight :: [Card] -> Bool
        canStraight = (<*>) ((&&) . (>= 3) . length) ((<*>) ((&&) . (<= 5) . length) (lift (&&) checkSuit (succession . sorted)))
            where
                rankC (Card _ rc) = rc
                sorted = sort . map rankC
                succession = (<*>) ((==) . (<*>) ((-) . fromEnum . last) (fromEnum . head)) (subtract 1 . length)

        -- Checks if a list of cards (a hand) is a Deadwood
        canDeadwood :: [Card] -> Bool
        canDeadwood cs = length cs == 1

        -- Checks if a list of cards (a hand) can form melds
        canMeld :: [Card] -> Bool
        canMeld = lift (||) canSet canStraight

        -- Determine all possible combinations of melds
        returnMelds :: [Card] -> [[Card]]
        returnMelds = filter canMeld . subsequences

        -- Construct all possible melds in a hand
        melds = foldr (\x xs ->
            if (canStraight x)
                then (convertStraight x): xs
            else if (canSet x)
                then (convertSet x): xs
            else if (canDeadwood x)
                then (convertDeadwood x): xs
            else xs
            ) [] cards
        -- Determine all the deadwoods once the all possible melds have been made
        meldedCards = getMelds $ returnMelds l
        cards = foldr (\a b -> if exists (join b) [a] then b else [a]: b) meldedCards l