string1 = "hello"
string2 = "world"
greeting = string1  ++ " " ++ string2

sqrt 3
max 5 7

max (5 + 2) (sqrt 17)

-- function defination

square x = x * x

multiMax a b x  = (max a b ) * x

posOrNeg x =
    if x >= 0
    then "Positive"
    else "Negative"


pow2 n = 
    if n == 0
    then 1
    else 2 * (pow2 (n-1))


repeatString str n = 
    if n == 0
    then ""
    else str ++ (repeatString str (n-1))


pow2 n = pow2loop n 1 0

pow2loop n x i = 
    if i < n
    then pow2loop n (x*2) (i+1)
    else x



x = [1,2,3]
empty = []
y = 0 : x -- [0, 1, 2, 3]
x' =  1 : ( 2 : (3 : []))

x'' = 1 : 2 : 3 : []

str = "abcde"

str' = 'a' : 'b' : 'c' : 'd' 

[1, 2, 3] ++ [ 4, 5]

[1,2,3,4,5]

"hello" ++ "world"

error = [1, "hello" ,2]

head [ 1, 2, 3] -- 1
tail [ 1, 2, 3] -- 2,3

head (tail [1, 2, 3]) -- 2

null [] -- True

null [1,2] -- False

double nums = 
    if null nums
    then []
    else (2 * (head nums)) : (double (tail nums))


removeOdd nums = 
    if null nums
    then []
    else 
        if (mod (head nums) 2) == 0 -- even?
        then (head nums) : (removeOdd (tail nums))
        else removeOdd (tail nums)


-- tuples
--

x = (1, "hello")
y = ("PI", 3.141592, [1,2,3], "four")

headAndLenght list =  (head list, length list)

fst (1, "hello")

snd (1, "hello")

-- Tuple Warning
-- Big tuples
-- Tuples spanning different parts of an application


-- Pattern Matching

fst' (a, b) = a
snd' (a, b) = b

-- Pattern Mathcing Lists
--
--
null' [] = True
null' (x:xs) = False


head' (x:xs) = x
head' [] = ?
head' [] =  error "head of empty list"


double [] = []
double (x:xs) = (2*x) : (double xs)


-- Guards
--

pow2 n 
    | n == 0    = 1
    | otherwise = 2 * (pw2 (n-1))


removeOdd [] = []
removeOdd (x:xs)
    | mod x 2 == 0     = x : (removeOdd xs)
    | otherwise        = removeOdd xs


-- Case Expressions
--
--

double nums = case nums of
    []   -> []
    (x : xs) -> (2*x) : (double xs)


anyEven nums = case (removeOdd nums) of
    [] -> False
    (x:xs -> True)


-- Let Binding
--
fancySeven = 
    let a = 3
    in 2 * a + 1

fancyInne = 
    let x = 4
        y = 5
    in x + y

numEven nums = 
    let evenNums = removeOdd nums
    in length evenNums


-- Where Binding
--
-
fancySeven = 2 * a + 1
    where a = 3


fancyNine = x + y
    where x = 4
          y = 5


-- "Where" goes with a functio definition
--

-- fancyten = 2 * (a + 1 where a = 4)
 fancyten = 2 * (let a = 4 in a + 1)


-- Where top down
-- Let - bottom up
--
--


-- Whitespaces
-- Do not use tabs, Ever-


-- indent further when breaking expression onto another line
pairMax p = max ( fst p)
                (snd p)

pairMax p = max ( fst p)
            (snd p)

pairMax p = max ( fst p)
    (snd p) -- error

-- Line up variable bindings
--


fancyNine = 
let x = 4
    y = 5
in x + y



fancyNine = 
let x = 4 
y = 5       -- error
in x + y


-- Lazy
--
--

foo (alpha 1)  (beta 2)



intsFrom n = n : (intsFrom (n + 1))

ints = intsFrom 1

take 10 ints



-- Higher Order Functions
pass3 f = f 3
-- pass3 sqrt

add1 x = x + 1

pass3 add1

compose f g x = f ( g x)

add1 x = x + 1
mult2 x = 2 * x

compose add1 mult2 4
-- 9   add1 ( mult2 4) = (2 * 4) + 1 = 9


always7 x = 7
always7' = const 7

(const 7) 5


-- Partial Application
--
--
foo x y z = x + y + z
foo_1_2 = foo 1 2
foo_1_2 3  -- 6


pass x f = f x
pass3 = pass 3



-- Operators
--
--

(+) 5 3

pass_3_4 f = f 3 4
pass_3_4 (+)

-- new operator definitions
(a, b) .+ (c,d) = (a+c, b+d)

plus1 = (+) 1
plus1' = (1+)
plus1'' = (+1)

-- Turning functions into operators
--
mod 10 2

10 `mod` 2 -- not single qoute ,backtick


-- Map
--

map length ["hello", "abc", "1234"]


map (1+) [1,3,4,5]


double = map (2*)
    

-- Filter
--

notNull xs = not (null xs)


filter notNull ["", "abc", "", "hello", ""]


isEven x = x `mod` 2 == 0

removeOdd = filter isEven


map snd (filter fst |[True, 1),(False, 7), (True, 11) ])


-- Fold
-- foldl


foldl (+) 0 [1,2,3,4]
-- 10 -- 0 + 1 + 2 + 3 + 4 = 10
--
--
--


showPlus s x = "(" ++ s ++ "+" ++ (show x) ++ ")"

--show functio nconvert number to sting


showPlus "(1+2)" 3

-- "((1+2)+3)"

foldl showPlus "0" [1,2,3,4]
--"((((0+1)+2)+3)+4)"


-- foldr
--
--
foldr (+) 0 [1,2,3,4]

-- 10 -- 1 + 2 + 3 + 4  + 0 == 10
--
--
--
--
--
showPlus'  x s = "("  ++ (show x) ++ "+" ++ s ++ ")"

foldr showPlus' "0" [1,2,3,4]

--"(1+(2+(3+(4+0))))"



foldl (-) 0 [1,2,3]
--(((0-1) - 2) - 3)


foldr (-) 0 [1,2,3]

-- 1 - (2 - (3 - 0)) = 2
--


-- foldl slighty faster
--

-- foldr infinite lists
--
--

-- Zip
--
--
zip [1,2,3] [4,5,6] -- [(1,4),(2,5),(3,6)]

zipWith  (+) [1,2,3] [4,5,6] -- [5,7,9]


plus3 x y z = x + y + z

zipWith3 plus3 [1,2,3] [4,5,6] [7,8,9]

--Lambda Expressions
--

-- \ menas lambda expression
zipWith3 (\ x y z -> x + y + z) [1,2,3] [4,5,6] [7,8,9]

map (\x -> 2 +x) [1,2,3]


map (2*) [1,2,3]

map (\x -> 2 * x + 1) [1,2,3]


-- Function operators
-- a . b a and b both must have only 1 arguments
--
--


(.) - Function Composition
($) - function Application


stringLength = length . show

stringLength' x = length ( show x) -- from right to left

notNull = not . null


f a b = a + b
g x = 2 * x

f . g


f $ x = f x

f $ g x = f (g x)

f $ g $ h $ k x = f ( g (h (k x)))

map (\f -> f 3) [(+1), (\x -> 2 * x + 3), (*2)] 
-- [4, 9 , 6]
--
map ($3) [(+1), (\x -> 2*x + 3), (*2)]
[4,9.6]

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter ( < p) xs
        greater = filter (>= p) xs

qsort' (p:xs) = qsort' [x | x<-xs, x<p] ++ [p] ++ qsort' [x | x<-xs, x>=p]


-- Type Ssytems

-- Statically typed
-- types are inferred
-- Don't have to write our explicit types
-- Explicit types communicate with PEOPLE, checked by compiler
--
--
-- Exploring Types in GHCI
-- :t -Print the type of  an expression
-- let z = 3
-- :t z  :: Num p => p
-- :t lines
--
--
-- Explicit Type

str :: [Char] -- not necessary
str = "hello"

-- Explicit Function Type
--
--
foo :: Int -> Int
foo x = 2 * x + 1

addInt3 :: Int -> INt -> Int -> Int
addInt3 x y z = x + y + z

-- Type Annotation
--

x = 3 :: Int
y = (3 :: Int) + (2.1 :: Double) -- error

y' = 3 + 2.1


-- When to use Explicit Types
-- communicating with people
--

mystery :: [Char] -> Int

-- Tracking down compiler errors
--
--

whats_wrong = x + y
    where x = length "hello"
          y = 6/2


whats_wrong' = x + y
    where x :: Int 
          x = length "hello"
          y :: Int
          y = 6/2
-- Help the complier

x = show (read "123") -- error
x' = show (read "123" :: Int)

-- Optimizing Performance
--

foo = x * y + z
    where x = 32
          y = 42
          z = -5

foo' :: Int
foo' = x * y + z
    where x = 32
          y = 42
          z = -5
-- not until you hvae to
--
-- Polymorphic Functions A function with a type variable
--

length_ints :: [Int] -> Int
length_ints [] = 0
length_ints (x:xs) = length_ints xs + 1

length_chars :: [char]  -> Int
length_ints [] = 0
length_chars (x:xs) = length_chars xs + 1


length [] = 0
length (x:xs) = length xs + 1

length :: [a] -> Int -- a is a placeholder for any type
length [] = 0
length (x:xs) = lenghh xs + 1


empty_lenght :: [a]
empty_lenght = []

list_double = 3.2 : empty
list_char = 'a' : empty_llist


head :: [a] => a
head (x:xs) => x

badHead [a]  -> b
badHead (x : xs) = x --error

ibad = (bad Head [1.2, 2, 4]) : "foo"


-- Type Class Constraints(约束)
length :: [a] -> Int
length [] -> 0
length (x:xs) = 1 + length xs


badSum [a] -> a
badSum [] = 0
badSum (x:xs) => x + sum xs


sum [] = 0
sum (x:xs) = x + sum xs


sum :: Num a => [a] -> a -- => me menas constraints of a, a must be a Num type 
sum [] = 0
sum (x : xs) = x + sum xs

show :: Show a => a -> String -- Show menas a  must be a type that has string representation

showSum :: (Num a, Show a) => [a] -> [Char]
showSum xs = show (sum xs)
3.1 :: Fractional p => p -- 分类


-- Custom Types
--

-- Type Synonyms(同意词)
--type String = [Char
--type Point = (Double, Double)
--
-- Newtype
-- Records
-- Algebraic(代数)) Data Types


midpoint :: (Double, Double) -> (Double, Double)  -> (Double, Double)

midpoint (x1, y1) (x2, y2) = 
    ((x1 + x2) / 2, (y1 + y2) /2)


midpoint' :: Point -> Point  -> Point

midpoint (x1, y1) (x2, y2) = 
    ((x1 + x2) / 2, (y1 + y2) /2)

p1 :: (Double, Double)
p1 = (1,2)
p2 :: Point
p2 = (3,4)

mid :: (Double, Double)
mid = midpoint p1 p2


-- New Type
--
--

newtype CustomeId = MakeCustomerId Int -- MakeCustomerId is the type constructor

badCustomer :: CustomerId
badCustomer = 13 -- error


custoemr :: CustomeId
customer = MakeCustomerId 13

cusatomerToInt :: CustomerId -> Int
cusatomerToInt (MakeCustomerId i) = i


newtype CustomerId = CustomerId Int

customer :: CustomerId
customer = CustomerId 13

customerToInt :: CustomerId -> Int
customerToInt (CustomerId i) = i

-- Records

data Customer = MakeCustomer
{
    customerId :: CustomerId
   ,name      :: String
   ,luckyNumber :: Int
}


alice :: Customer

alice = MakeCustomer 
{ customerId = MakeCustomerId 13
    , name   = "Alice"
    , luckyNumber = 42
}

customerId alice -- get record field customerId

-- update Recores (creaqte a new record)

sally = alice {name = "Sally", luckyNumber = 33}

-- Records are Not extensible
-- No shared field names, two records can't have the same name fields

data Customer = Customer 
{name :: String
,customerId :: CustomerId
}
data Supplier = Supplier 
{name :: String
,supplierId :: SupplierId
}

--Algebraic Data Types
--
--

data Customer = MakeCustomer CustomerId String int
data Customer = Customer CustomerId String int

alice :: Customer
alice = Customer (CustomerId 13) "Alice" 42

getCustoemrId :: Custoemr -> CustoemrId
getCustoemrId (Custoemr cust id name luckerNumber) = cust_id
getCustoemrId (Custoemr cust_id _ _) = cust_id

-- Newtype, but with more arguments
--

data Customer = Customer CustomerId String Int

newtype CustomerId = CustoemrId Int

-- Tuples, but with names
--

x :: (Double, Double, Double)

data RGB = RGB Double Double Double
x :: RGB

data StringThree = StringThree String [StringTree]

hierarchy = StringTree "C:"
            [
                StringTree "Program Files" []
              , StringTree "Users"
                   [StringTree "Alices" []]
              , StringTree "Cats" []
            ]

 
-- Algebraic Data Type Constructors
--

data Bool = False | True

x :: Bool
x = False
y :: Bool
y = True


negate :: Bool -> Bool
negate True = False
negate False = True


data DialogResponse = Yes | No | Help  | Quit

data MaybeInt = NoInt | JustInt Int

defaultInt :; Int -> MaybeInt -> Int
defaultInt defaultValue NotInt  = defaultValue
defaultInt _ (JustInt x) = x

data StringList = EmptyStringList
                | ConsStringList String StringList


lengthStringList :: StringList -> INt
lengthStringList EmptyStringList = 0

lengthStringList (ConsStringList _ xs) =
    1 + lengthStringList xs


length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + lenght xs

-- Parameterized Types
--
--

data Maybe a = Just a | Nothing
x :: Maybe Int
x = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal Nothing = defaultVal
fromMaybe _ (Just x) = x


data List a = Empty | Cons a (List a)

data Map k a = ...


--Tyep Classes
--

--Type Class Instances
--
--

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x ( y : ys)
    | x == y = True
    | otherwise = elem x ys



data RGB = RGB Int Int Int

colors = [RGB 255 0 0 , RGB 0 255 0, RGB 0 0 255]
green = RGB 0 255 0
greenInColors = elem green colors

instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) = 
    (r1 == r2) && (g1 == g2) && (b1 == b2)



-- Type Class Instances for Parameterized Types
--

data Maybe' a = Nothing' | Just' a


instance Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) = (Just' y) = x == y

instance (Eq a) =>  (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == (Just' _) = False
    (Just' _) == Nothing' = False
    (Just' x) = (Just' y) = x == y



--Deriving Type Class instances
--

data RGB = RGB Int Int Int
instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) = 
        (r1 == r2) && (g1 == g2) && (b1 == b2)


data Person = Person String Int Int
instance Eq Person where
(PErson name1 age1 height1) == 
  (Person name2 age2 height2) = 
    (name1 == name2) && (age1 == age2) &&
    (height1 == height2)


data RGB = RGB Int Int Int
    deriving Eq

---Deriving Type Class Instances
--
--Eq
--  Deriving - components-wise equality
--Ord
--  (<),(>),(<=), (>=)
--  Deriving - component-wise comparison
--Show
--  show
--  Deriving-'{Constructor-name}{argument-1}{argument2}...
--Read
--  Read
--  Deriving-parse output of default show
--
--
--Subclasses of Type Classes
--

class (Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    compare :: a -> a -> Ordering
    max :: a -> a -> a
    min :: a -> a -> a

data  Ordering = LT | EQ | GT

-- Mini`mum complete definition : compare or (<=)


data Point2 = Point2 Double Double
deriving Show
data Point3 = Point3 Double Double Double
deriving Show


class Measurable a where
distance :: a -> a -> Double

class (Measurable a, Show a) -> Direction a where
getDirections :: a -> a -> String

getDirections p1 p2 =
    "Go from " ++ (show p1) ++
    " towards" ++ (show p2) ++ 
    " and stop after " ++ (show (distance p1 p2))


instance Directions Point3 where
getDirections p1 p2 = 
    "Fly from" ++ (show p1) ++
    " towards" ++ (show p2) ++ 
    " and stop after " ++ (show (distance p1 p2))


instance Directions Point2 where

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ biggerSorted


data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show
