import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "isCar" $ do
    it "returns True for a Car" $ do
      isCar (Car (Price 10000)) `shouldBe` True

    it "returns False for a Plane" $ do
      isCar (Plane (Price 1000000)) `shouldBe` False

  describe "areCars" $ do
    it "returns True for an empty list" $ do
      areCars [] `shouldBe` True

    it "returns True for a list containing only Cars" $ do
      areCars [Car (Price 10000), Car (Price 20000)] `shouldBe` True

    it "returns False for a list containing at least one Plane" $ do
      areCars [Car (Price 10000), Plane (Price 1000000), Car (Price 20000)] `shouldBe` False

    it "returns False for a list containing only Planes" $ do
      areCars [Plane (Price 1000000), Plane (Price 2000000)] `shouldBe` False

  describe "BinaryTree creation" $ do
    it "creates an empty tree (Leaf)" $ do
      let emptyTree = Leaf :: BinaryTree Int
      emptyTree `shouldBe` Leaf

    it "creates a single node tree" $ do
      let singleNode = Node Leaf 5 Leaf
      singleNode `shouldBe` Node Leaf 5 Leaf

    it "creates a tree with multiple levels" $ do
      let tree = Node (Node Leaf 2 Leaf) 5 (Node Leaf 8 Leaf)
      tree `shouldBe` Node (Node Leaf 2 Leaf) 5 (Node Leaf 8 Leaf)

  describe "insertTree" $ do
    it "inserts into an empty tree" $ do
      insertTree 5 Leaf `shouldBe` Node Leaf 5 Leaf

    it "inserts a smaller value to the left" $ do
      let tree = Node Leaf 5 Leaf
      insertTree 3 tree `shouldBe` Node (Node Leaf 3 Leaf) 5 Leaf

    it "inserts a larger value to the right" $ do
      let tree = Node Leaf 5 Leaf
      insertTree 7 tree `shouldBe` Node Leaf 5 (Node Leaf 7 Leaf)

    it "does not insert duplicate values" $ do
      let tree = Node Leaf 5 Leaf
      insertTree 5 tree `shouldBe` Node Leaf 5 Leaf

    it "inserts multiple values in sequence" $ do
      let tree = insertTree 3 $ insertTree 7 $ insertTree 5 Leaf
      tree `shouldBe` Node (Node Leaf 3 Leaf) 5 (Node Leaf 7 Leaf)

    it "builds a complex tree with multiple insertions" $ do
      let tree = foldl (flip insertTree) Leaf [5, 3, 7, 2, 4, 6, 8]
      let expected = Node 
                      (Node 
                        (Node Leaf 2 Leaf) 
                        3 
                        (Node Leaf 4 Leaf)) 
                      5 
                      (Node 
                        (Node Leaf 6 Leaf) 
                        7 
                        (Node Leaf 8 Leaf))
      tree `shouldBe` expected

    it "maintains BST property after insertions" $ do
      let tree = foldl (flip insertTree) Leaf [5, 3, 7, 1, 4, 6, 9]
      -- We can't easily test BST property directly with shouldBe,
      -- but we can test specific structural expectations
      case tree of
        Node left 5 right -> do
          case left of
            Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf) -> return ()
            _ -> expectationFailure "Left subtree structure incorrect"
          case right of
            Node (Node Leaf 6 Leaf) 7 (Node Leaf 9 Leaf) -> return ()
            _ -> expectationFailure "Right subtree structure incorrect"
        _ -> expectationFailure "Tree should have 5 as root"

  describe "BinaryTree with different types" $ do
    it "works with Char values" $ do
      let tree = foldl (flip insertTree) Leaf ['d', 'b', 'f', 'a', 'c', 'e', 'g']
      let expected = Node 
                      (Node 
                        (Node Leaf 'a' Leaf) 
                        'b' 
                        (Node Leaf 'c' Leaf)) 
                      'd' 
                      (Node 
                        (Node Leaf 'e' Leaf) 
                        'f' 
                        (Node Leaf 'g' Leaf))
      tree `shouldBe` expected

    it "works with String values" $ do
      let tree = insertTree "hello" $ insertTree "world" Leaf
      tree `shouldBe` Node (Node Leaf "hello" Leaf) "world" Leaf

  describe "Functor instance (fmap)" $ do
    it "maps over an empty tree" $ do
      fmap (+1) Leaf `shouldBe` Leaf

    it "maps over a single node" $ do
      let tree = Node Leaf 5 Leaf
      fmap (*2) tree `shouldBe` Node Leaf 10 Leaf

    it "maps over a simple tree" $ do
      let tree = Node (Node Leaf 2 Leaf) 5 (Node Leaf 8 Leaf)
      fmap (+1) tree `shouldBe` Node (Node Leaf 3 Leaf) 6 (Node Leaf 9 Leaf)

  describe "Tree Traversals" $ do
    let emptyTree = Leaf :: BinaryTree Int
    let singleNode = Node Leaf 5 Leaf
    let complexTree = Node (Node Leaf 2 Leaf) 5 (Node Leaf 8 Leaf)
    
    describe "treeToList with Preorder" $ do
      it "returns empty list for empty tree" $ do
        treeToList Preorder emptyTree `shouldBe` []

      it "returns single element for single node" $ do
        treeToList Preorder singleNode `shouldBe` [5]

      it "returns preorder sequence for complex tree" $ do
        treeToList Preorder complexTree `shouldBe` [5, 2, 8]

    describe "treeToList with Inorder" $ do
      it "returns empty list for empty tree" $ do
        treeToList Inorder emptyTree `shouldBe` []

      it "returns single element for single node" $ do
        treeToList Inorder singleNode `shouldBe` [5]

      it "returns sorted sequence for BST" $ do
        treeToList Inorder complexTree `shouldBe` [2, 5, 8]

    describe "treeToList with Postorder" $ do
      it "returns empty list for empty tree" $ do
        treeToList Postorder emptyTree `shouldBe` []

      it "returns single element for single node" $ do
        treeToList Postorder singleNode `shouldBe` [5]

      it "returns postorder sequence for complex tree" $ do
        treeToList Postorder complexTree `shouldBe` [2, 8, 5]

    describe "traversal comparison" $ do
      it "shows different orderings for the same tree" $ do
        let tree = insertTree 3 $ insertTree 7 $ insertTree 1 $ insertTree 9 $ insertTree 5 Leaf
        treeToList Preorder tree `shouldBe` [5, 1, 3, 9, 7]
        treeToList Inorder tree `shouldBe` [1, 3, 5, 7, 9]
        treeToList Postorder tree `shouldBe` [3, 1, 7, 9, 5]

  describe "Sum type mappend operations" $ do
    
    describe "Semigroup laws" $ do
      it "satisfies associativity: (a <> b) <> c = a <> (b <> c)" $ do
        let a = LeftData "hello" :: Sum String String
        let b = LeftData " " :: Sum String String  
        let c = LeftData "world" :: Sum String String
        (a <> b) <> c `shouldBe` a <> (b <> c)

    describe "LeftData <> LeftData combinations" $ do
      it "combines two LeftData values using the left type's semigroup" $ do
        let left1 = LeftData "Hello" :: Sum String String
        let left2 = LeftData " World" :: Sum String String
        left1 <> left2 `shouldBe` LeftData "Hello World"

      it "combines LeftData with numeric types" $ do
        let left1 = LeftData [1, 2] :: Sum [Int] [String]
        let left2 = LeftData [3, 4] :: Sum [Int] [String]
        left1 <> left2 `shouldBe` LeftData [1, 2, 3, 4]

    describe "RightData <> RightData combinations" $ do
      it "combines two RightData values using the right type's semigroup" $ do
        let right1 = RightData "Hello" :: Sum String String
        let right2 = RightData " World" :: Sum String String
        right1 <> right2 `shouldBe` RightData "Hello World"

      it "combines RightData with list types" $ do
        let right1 = RightData [10, 20] :: Sum [String] [Int]
        let right2 = RightData [30, 40] :: Sum [String] [Int]
        right1 <> right2 `shouldBe` RightData [10, 20, 30, 40]

    describe "LeftData <> RightData combinations" $ do
      it "RightData takes precedence: LeftData <> RightData = RightData" $ do
        let left = LeftData "ignored" :: Sum String String
        let right = RightData "result" :: Sum String String
        left <> right `shouldBe` RightData "result"

      it "works with different types" $ do
        let left = LeftData [1, 2, 3] :: Sum [Int] String
        let right = RightData "chosen" :: Sum [Int] String
        left <> right `shouldBe` RightData "chosen"

    describe "RightData <> LeftData combinations" $ do
      it "RightData takes precedence: RightData <> LeftData = RightData" $ do
        let right = RightData "result" :: Sum String String
        let left = LeftData "ignored" :: Sum String String
        right <> left `shouldBe` RightData "result"

      it "works with different types" $ do
        let right = RightData "chosen" :: Sum [Int] String
        let left = LeftData [1, 2, 3] :: Sum [Int] String
        right <> left `shouldBe` RightData "chosen"

    describe "Monoid properties" $ do
      it "mempty is LeftData mempty" $ do
        (mempty :: Sum String String) `shouldBe` LeftData ""

      it "left identity: mempty <> x = x" $ do
        let x = RightData "hello" :: Sum String String
        mempty <> x `shouldBe` x

      it "right identity: x <> mempty = x" $ do
        let x = RightData "hello" :: Sum String String
        x <> mempty `shouldBe` x

      it "works with LeftData and mempty" $ do
        let x = LeftData "test" :: Sum String String
        x <> mempty `shouldBe` LeftData "test"
        mempty <> x `shouldBe` LeftData "test"

    describe "Complex scenarios" $ do
      it "chains multiple Sum operations" $ do
        let a = LeftData "A" :: Sum String String
        let b = LeftData "B" :: Sum String String
        let c = RightData "C" :: Sum String String
        let d = LeftData "D" :: Sum String String
        a <> b <> c <> d `shouldBe` RightData "C"

      it "works with Optional types inside Sum" $ do
        let left1 = LeftData (Only "Hello") :: Sum (Optional String) (Optional String)
        let left2 = LeftData (Only " World") :: Sum (Optional String) (Optional String)
        left1 <> left2 `shouldBe` LeftData (Only "Hello World")

      it "handles empty Optional in Sum" $ do
        let left1 = LeftData Nada :: Sum (Optional String) (Optional String)
        let left2 = LeftData (Only "World") :: Sum (Optional String) (Optional String)
        left1 <> left2 `shouldBe` LeftData (Only "World")

  describe "Exclamation, Adverb, and String Semigroup operations" $ do
    
    describe "Exclamation Semigroup" $ do
      it "concatenates Exclamation values directly" $ do
        let exc1 = Exclamation "Hello"
        let exc2 = Exclamation "World"
        exc1 <> exc2 `shouldBe` Exclamation "HelloWorld"

      it "works with empty Exclamation" $ do
        let exc = Exclamation "Test"
        let empty = Exclamation ""
        exc <> empty `shouldBe` Exclamation "Test"
        empty <> exc `shouldBe` Exclamation "Test"

      it "satisfies associativity" $ do
        let a = Exclamation "A"
        let b = Exclamation "B"
        let c = Exclamation "C"
        (a <> b) <> c `shouldBe` a <> (b <> c)

      it "has mempty as identity" $ do
        let exc = Exclamation "Hello!"
        exc <> mempty `shouldBe` exc
        mempty <> exc `shouldBe` exc

    describe "Adverb Semigroup" $ do
      it "concatenates Adverb values with space separator" $ do
        let adv1 = Adverb "quickly"
        let adv2 = Adverb "carefully"
        adv1 <> adv2 `shouldBe` Adverb "quickly carefully"

      it "handles multiple adverbs" $ do
        let adv1 = Adverb "very"
        let adv2 = Adverb "slowly"
        let adv3 = Adverb "indeed"
        adv1 <> adv2 <> adv3 `shouldBe` Adverb "very slowly indeed"

      it "works with empty Adverb" $ do
        let adv = Adverb "silently"
        let empty = Adverb ""
        adv <> empty `shouldBe` Adverb "silently"
        empty <> adv `shouldBe` Adverb "silently"

      it "satisfies associativity" $ do
        let a = Adverb "first"
        let b = Adverb "second"
        let c = Adverb "third"
        (a <> b) <> c `shouldBe` a <> (b <> c)

      it "has mempty as identity" $ do
        let adv = Adverb "beautifully"
        adv <> mempty `shouldBe` adv
        mempty <> adv `shouldBe` adv

    describe "String Semigroup" $ do
      it "concatenates strings directly" $ do
        let str1 = "Hello"
        let str2 = "World"
        str1 <> str2 `shouldBe` "HelloWorld"

      it "works with spaces" $ do
        let str1 = "Hello "
        let str2 = "World!"
        str1 <> str2 `shouldBe` "Hello World!"

      it "satisfies associativity" $ do
        let a = "A"
        let b = "B"
        let c = "C"
        (a <> b) <> c `shouldBe` a <> (b <> c)

      it "has mempty as identity" $ do
        let str = "Test"
        str <> mempty `shouldBe` str
        mempty <> str `shouldBe` str

    describe "Mixed Semigroup operations" $ do
      it "combines different types using their respective rules" $ do
        let exc1 = Exclamation "Wow"
        let exc2 = Exclamation "!"
        let adv1 = Adverb "very"
        let adv2 = Adverb "loudly"
        
        -- Exclamation concatenation
        exc1 <> exc2 `shouldBe` Exclamation "Wow!"
        
        -- Adverb concatenation with space
        adv1 <> adv2 `shouldBe` Adverb "very loudly"

      it "works with Sum types containing Exclamation and Adverb" $ do
        let sumExc1 = LeftData (Exclamation "Hello") :: Sum Exclamation Adverb
        let sumExc2 = LeftData (Exclamation "!") :: Sum Exclamation Adverb
        sumExc1 <> sumExc2 `shouldBe` LeftData (Exclamation "Hello!")

      it "works with Sum types - mixed LeftData and RightData" $ do
        let sumExc = LeftData (Exclamation "Hello") :: Sum Exclamation Adverb
        let sumAdv = RightData (Adverb "quickly") :: Sum Exclamation Adverb
        sumExc <> sumAdv `shouldBe` RightData (Adverb "quickly")

      it "chains multiple operations" $ do
        let exc = Exclamation "Yes"
        let adv1 = Adverb "absolutely"
        let adv2 = Adverb "definitely"
        let str = "confirmed"
        
        -- Test individual combinations
        exc <> Exclamation "!" `shouldBe` Exclamation "Yes!"
        adv1 <> adv2 `shouldBe` Adverb "absolutely definitely"
        str <> "!" `shouldBe` "confirmed!"

    describe "Monoid laws verification" $ do
      it "verifies left identity for all types" $ do
        let exc = Exclamation "test"
        let adv = Adverb "slowly"
        let str = "example"
        
        mempty <> exc `shouldBe` exc
        mempty <> adv `shouldBe` adv
        mempty <> str `shouldBe` str

      it "verifies right identity for all types" $ do
        let exc = Exclamation "test"
        let adv = Adverb "slowly"
        let str = "example"
        
        exc <> mempty `shouldBe` exc
        adv <> mempty `shouldBe` adv
        str <> mempty `shouldBe` str

      it "verifies mempty values" $ do
        (mempty :: Exclamation) `shouldBe` Exclamation ""
        (mempty :: Adverb) `shouldBe` Adverb ""
        (mempty :: String) `shouldBe` ""

  describe "MySemigroup typeclass tests" $ do
    
    describe "MySemigroup Exclamation" $ do
      it "concatenates Exclamation values using myAppend" $ do
        let exc1 = Exclamation "Hello"
        let exc2 = Exclamation "World"
        myAppend exc1 exc2 `shouldBe` Exclamation "HelloWorld"

      it "handles empty exclamations" $ do
        let exc = Exclamation "Test"
        let empty = Exclamation ""
        myAppend exc empty `shouldBe` Exclamation "Test"
        myAppend empty exc `shouldBe` Exclamation "Test"

      it "chains multiple myAppend operations" $ do
        let exc1 = Exclamation "A"
        let exc2 = Exclamation "B"
        let exc3 = Exclamation "C"
        myAppend (myAppend exc1 exc2) exc3 `shouldBe` Exclamation "ABC"

      it "satisfies associativity property" $ do
        let a = Exclamation "X"
        let b = Exclamation "Y"
        let c = Exclamation "Z"
        myAppend (myAppend a b) c `shouldBe` myAppend a (myAppend b c)

    describe "MySemigroup Adverb" $ do
      it "concatenates Adverb values with space using myAppend" $ do
        let adv1 = Adverb "quickly"
        let adv2 = Adverb "carefully"
        myAppend adv1 adv2 `shouldBe` Adverb "quickly carefully"

      it "handles empty adverbs properly" $ do
        let adv = Adverb "slowly"
        let empty = Adverb ""
        myAppend adv empty `shouldBe` Adverb "slowly"
        myAppend empty adv `shouldBe` Adverb "slowly"

      it "chains multiple adverbs correctly" $ do
        let adv1 = Adverb "very"
        let adv2 = Adverb "slowly"
        let adv3 = Adverb "indeed"
        myAppend (myAppend adv1 adv2) adv3 `shouldBe` Adverb "very slowly indeed"

      it "satisfies associativity property" $ do
        let a = Adverb "first"
        let b = Adverb "second"
        let c = Adverb "third"
        myAppend (myAppend a b) c `shouldBe` myAppend a (myAppend b c)

      it "handles both empty operands" $ do
        let empty1 = Adverb ""
        let empty2 = Adverb ""
        myAppend empty1 empty2 `shouldBe` Adverb ""

    describe "MySemigroup vs Semigroup comparison" $ do
      it "MySemigroup and Semigroup should behave the same for Exclamation" $ do
        let exc1 = Exclamation "Hello"
        let exc2 = Exclamation "World"
        myAppend exc1 exc2 `shouldBe` (exc1 <> exc2)

      it "MySemigroup and Semigroup should behave the same for Adverb" $ do
        let adv1 = Adverb "very"
        let adv2 = Adverb "quickly"
        myAppend adv1 adv2 `shouldBe` (adv1 <> adv2)

      it "handles empty cases consistently between both typeclasses" $ do
        let exc = Exclamation "test"
        let excEmpty = Exclamation ""
        let adv = Adverb "slowly"
        let advEmpty = Adverb ""
        
        -- Exclamation consistency
        myAppend exc excEmpty `shouldBe` (exc <> excEmpty)
        myAppend excEmpty exc `shouldBe` (excEmpty <> exc)
        
        -- Adverb consistency
        myAppend adv advEmpty `shouldBe` (adv <> advEmpty)
        myAppend advEmpty adv `shouldBe` (advEmpty <> adv)

    describe "MySemigroup properties and laws" $ do
      it "verifies closure property" $ do
        let exc1 = Exclamation "A"
        let exc2 = Exclamation "B"
        let result = myAppend exc1 exc2
        -- Result should be of the same type (this is guaranteed by the type system)
        result `shouldBe` Exclamation "AB"

      it "demonstrates non-commutativity" $ do
        let exc1 = Exclamation "Hello"
        let exc2 = Exclamation "World"
        let adv1 = Adverb "quickly"
        let adv2 = Adverb "slowly"
        
        -- Show that order matters
        myAppend exc1 exc2 `shouldNotBe` myAppend exc2 exc1
        myAppend adv1 adv2 `shouldNotBe` myAppend adv2 adv1

      it "works with complex nested operations" $ do
        let exc1 = Exclamation "Start"
        let exc2 = Exclamation "Middle"  
        let exc3 = Exclamation "End"
        let adv1 = Adverb "very"
        let adv2 = Adverb "carefully"
        let adv3 = Adverb "now"
        
        -- Complex exclamation chain
        let excResult = myAppend exc1 (myAppend exc2 exc3)
        excResult `shouldBe` Exclamation "StartMiddleEnd"
        
        -- Complex adverb chain
        let advResult = myAppend adv1 (myAppend adv2 adv3)
        advResult `shouldBe` Adverb "very carefully now"

    describe "MySemigroup edge cases" $ do
      it "handles special characters in Exclamation" $ do
        let exc1 = Exclamation "Hello!"
        let exc2 = Exclamation "@#$%"
        myAppend exc1 exc2 `shouldBe` Exclamation "Hello!@#$%"

      it "handles special characters in Adverb" $ do
        let adv1 = Adverb "very"
        let adv2 = Adverb "fast!"
        myAppend adv1 adv2 `shouldBe` Adverb "very fast!"

      it "handles whitespace correctly in Adverb" $ do
        let adv1 = Adverb "quite"
        let adv2 = Adverb "slowly indeed"
        myAppend adv1 adv2 `shouldBe` Adverb "quite slowly indeed"
