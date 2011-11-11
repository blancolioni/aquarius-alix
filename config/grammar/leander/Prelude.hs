module Prelude where

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!, `seq`

type String = [Char]

head :: [a] -> a
head [] = error "head: empty list"
head (x:xs) = x

tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (x:xs) = xs

last :: [a] -> a
last [] = error "last: empty list"
last (x:xs) = if null xs then x else last xs

init :: [a] -> [a]
init [] = error "init : empty list"
init (x:xs) = if null xs then [] else x : init xs

null :: [a] -> Bool
null [] = True
null (x:xs) = False

const :: a -> b -> a
const x y = x

(.) f g x = f (g x)

(++) :: [a] -> [a] -> [a]
(++) []     = id
(++) (x:xs) = \ ys -> x : (xs ++ ys)

(!!) xs n = head . drop n $ xs

--  tails :: [a] -> [[a]]
--  tails []    = [[]]
--  tails (x:xs) = (x:xs) : tails xs

class Eq a where
   (==), (/=) :: a -> a -> Bool
   x == y = not (x /= y)
   x /= y = not (x == y)

-- instance Eq a => Eq [a] where
--     [] == []
--     (x:xs) == (y:ys) = x == y && xs == ys

not :: Bool -> Bool
not False = True
not True = False

(&&), (||) :: Bool -> Bool -> Bool
(&&) True  x       =  x
(&&) False _       =  False
(||) True _       =  True
(||) False x       =  x

length [] = 0
length (x:xs) = 1 + length xs

take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n - 1) xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

sum [] = 0
sum (x:xs) = x + sum xs

cycle xs = xs ++ cycle xs

repeat x = x : repeat x

replicate n = take n . repeat

reverse :: [a] -> [a]
reverse = rvs []
    where rvs acc [] = acc
          rvs acc (x:xs) = rvs (x:acc) xs

elem x [] = False
elem x (y:ys) = x == y || elem x ys

data Ordering = LT | EQ | GT
          deriving (Eq, Ord)

testEq = compare 1 2

data Ratio a = Rat a a

type Rational = Ratio Integer

class (Eq a) => Ord a where
    compare :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                       else GT

    x <= y = compare x y /= GT
    x <  y = compare x y == LT
    x >= y = compare x y /= LT
    x >  y = compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y

-- Enumeration and Bounded classes


class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . (+1) . fromEnum
    pred             =  toEnum . (subtract 1) . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]


class  Bounded a  where
    minBound         :: a
    maxBound         :: a


class (Eq a) => Num a where
    (+), (-), (*) :: a -> a -> a
    negate        :: a -> a
    fromInteger   :: Integer -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x

class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational


class  (Real a, Enum a) => Integral a  where
--    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    toInteger        :: a -> Integer
--    quotRem, divMod  :: a -> a -> (a,a)

instance Integral Int where
    div = primIntDiv
    mod = primIntMod
    toInteger = primIntToInteger

instance Real Int where
    toRational x = Rat (toInteger x) 1

        -- Minimal complete definition:
        --      quotRem, toInteger
--     n `quot` d       =  q  where (q,r) = quotRem n d
--     n `rem` d        =  r  where (q,r) = quotRem n d
--     n `div` d        =  q  where (q,r) = divMod n d
--     n `mod` d        =  r  where (q,r) = divMod n d

subtract m n = n - m

instance Num Int where
    (+) = primIntPlus
    (-) = primIntMinus
    (*) = primIntMult
    fromInteger = primIntFromInteger

instance Enum Int where
    toEnum = id
    fromEnum = id
    succ = (+1)
    pred = (-1)
    enumFrom x = x : enumFrom (x + 1)
    enumFromTo x y = if x <= y
                     then x : enumFromTo (x + 1) y
                     else []
    enumFromThen x y = x : enumFromThen y (y + y - x)
    enumFromThenTo x y z = if x <= z
                           then x : enumFromThenTo y (y + y - x) z
                           else []

class  Monad m  where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a
    fail   :: String -> m a

        -- Minimal complete definition:
        --      (>>=), return
    m >> k  =  m >>= \_ -> k
    fail s  = error s

id :: a -> a
id x = x

data Maybe a = Nothing | Just a

bindJust Nothing = const Nothing
bindJust (Just x) = \f -> f x

instance Monad Maybe where
    return = Just
    (>>=) = bindJust

class  Show a  where
    showsPrec        :: Int -> a -> String -> String
    show             :: a -> String
    showList         :: [a] -> String -> String

        -- Mimimal complete definition:
        --      show or showsPrec
    showsPrec n x s   = show x ++ s

    show x            = showsPrec 0 x ""

    showList []       = showString "[]"
    showList (x:xs)   = showChar '[' . shows x . showl xs
                        where showl []     = showChar ']'
                              showl (x:xs) = showChar ',' . shows x .
                                             showl xs


shows            =  showsPrec 0

showString = (++)
showChar = (:)

($) :: (a -> b) -> a -> b
f $ x = f x

putChar :: Char -> IO ()
putChar ch = IO (\w -> (,) () (primPutChar w ch))

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = putChar x >> putStr xs

putStrLn :: String -> IO ()
putStrLn line = putStr (line ++ "\n")

print = putStrLn . show

--  data IO a = IO (() -> (a,()))

--  returnIO :: a -> IO a
returnIO x = IO (\w -> (x, w))

bindIO a1 a2 = IO (\ w0 -> (\ w1 -> case a2 (fst w1) of
                                      (IO io2) -> io2 (snd w1))
                   (case a1 of
                      IO ioa1 -> ioa1 w0))

fst (x,y) = x
snd (x,y) = y

-- snd tpl = case tpl of
--              (x,y) -> y


instance Monad IO where
   return = returnIO
   (>>=) = bindIO

--  runIO :: IO a -> a
runIO action = case action of
                 (IO upd) -> case upd 99 of
                               (x,y) -> y

unmatched_pattern = error "unmatched pattern"

primIntFromInteger :: Integer -> Int
primIntFromInteger = primitive #I

primIntToInteger :: Int -> Integer
primIntToInteger = primitive #I

primIntPlus, primIntMinus, primIntMult, primIntMod, primIntDiv :: Int -> Int -> Int
primIntPlus = primitive #intPlus
primIntMinus = primitive #intMinus
primIntMult = primitive #intMult
primIntMod = primitive #intMod
primIntDiv = primitive #intDiv

instance Show Bool where
    show False = "False"
    show True  = "True"

instance Show Int where
    show = showInt

instance Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs



showl ""       = showChar '"'
showl ('"':cs) = showString "\\\"" . showl cs
showl (c:cs)   = showLitChar c . showl cs

showLitChar = showChar

showInt :: Int -> String
showInt n = if n < 10
            then [toEnum (n + 48)]
            else showInt (n `div` 10) ++ showInt (n `mod` 10)

instance (Show a) => Show [a] where
    showsPrec p      = showList
