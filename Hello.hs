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



