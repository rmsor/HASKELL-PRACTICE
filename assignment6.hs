import Data.Maybe

data Tree a= Node  (Maybe a)   (Maybe (Tree a))  (Maybe (Tree a)) deriving (Show,Eq)

myTNS=Node (Just 5) (Just(Node (Just 2) Nothing Nothing)) (Just(Node (Just 2) Nothing Nothing))

sumTree (Node (Just a) Nothing Nothing)=a
sumTree (Node Nothing Nothing Nothing)=0
sumTree (Node (Just a) (Just lt) (Just rt))=a+sumTree lt+sumTree rt

depthTree (Node Nothing Nothing Nothing)=0
depthTree (Node (Just a) Nothing Nothing)=1
depthTree (Node (Just a) (Just lt) (Just rt))=1+max  (depthTree lt)  (depthTree rt)

isBalancedTree (Node Nothing Nothing Nothing)=False
isBalancedTree (Node (Just a) (Just lt) (Just rt))=depthTree lt==depthTree rt

countNodes (Node (Just a) Nothing Nothing)=1
countNodes (Node Nothing Nothing Nothing)=0
countNodes (Node (Just a) (Just lt) (Just rt))=1+countNodes lt+countNodes rt

main=do 
      putStrLn "-----------sum of tree--------------"
      print $ sumTree myTNS
      putStrLn "-----------depth of tree-------------"
      print $ depthTree myTNS
      putStrLn "-----------is balanced  tree-------------"
      print $ isBalancedTree myTNS
      putStrLn "-----------count Nodes-------------"
      print $ countNodes myTNS
