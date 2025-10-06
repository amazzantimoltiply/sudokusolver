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
