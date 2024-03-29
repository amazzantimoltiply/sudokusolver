
module RegisteredUser where
    newtype Username = Username String
    newtype Accountnumber = Accountnumber Integer
    data User = UnregisteredUser | RegisteredUser Username Accountnumber
    printUser::User -> IO()
    printUser UnregisteredUser = putStrLn "Unregistered User"
    printUser (RegisteredUser(Username name)
        (Accountnumber acnum))
            = putStrLn $ name ++ " " ++ show acnum

    data WherePenguinsLive = 
        Galapagos
        | Antartica
        | Australia
        | SouthAfrica
        | SouthAmerica
        deriving (Eq,Show)

    data Penguin = 
        Peng WherePenguinsLive
        deriving (Eq,Show)

    isSouthAfrica::WherePenguinsLive -> Bool
    isSouthAfrica SouthAfrica = True
    isSouthAfrica _ = False

    greetIfCool::String -> IO()
    greetIfCool coolness = 
        case cool of
            True -> putStrLn "You're cool mate!"
            False -> putStrLn "Not cool man!"
        where cool = coolness == "Coolio"

    functionC x y = 
        case (x>y) of
            True -> x
            False -> y
    
    ifEvenAdd2 n =
        case even n of
            True -> (n+2)
            False -> n

    main = do
        let myUser = Username "Andrea"
        let myAccNum = Accountnumber 1
        printUser $ RegisteredUser myUser myAccNum
