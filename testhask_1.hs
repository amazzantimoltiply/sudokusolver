
module TestAssertion where
import Language.Haskell.TH (Lit(IntegerL))
import GHCi.Message (THResultType)

class Numberish a where
    fromNumber::Integer->a
    toNumber::a->Integer

data Option = Some | None deriving (Show)
newtype Age = Age Integer deriving (Eq,Show)


instance Numberish Age where
    toNumber (Age n) = n
    fromNumber = Age
newtype Year= Year Integer deriving (Eq,Show)
instance Numberish Year  where
    toNumber (Year n) = n
    fromNumber = Year


sumNumberish:: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA    = toNumber a
          integerOfAPrime = toNumber a'
          summed = integerOfA + integerOfAPrime

data Person = Person Bool deriving Show
printPerson::Person -> IO()
printPerson person = print person

data Mood = Blah | Woot deriving (Show,Eq,Ord)
settleDown x = if x == Woot then Blah else x
chk::Eq b => (a->b)->a->b->Bool
chk f x y = f x ==y

bindExp :: Integer -> String
bindExp x = let y = x+1 in
            "the integer was: " ++ show x
             ++ " and y was: " ++ show y

addFive::(Num a, Ord a, Ord a)=> a-> a -> a
addFive x y = min x y + 5
mTh x = \y -> \z -> x * y * z

mFlip f x y = f y x
pt::Integer-> Bool
pt 2 = True
pt 1 = True
pt _ =False

main = do 
    pt 2
    
    


