import Data.List
import Test.QuickCheck
data Tree a=Null|Node a (Tree a) (Tree a) deriving (Show,Eq)
myTree=Node 7 (Node 4 (Node 3 Null Null) (Node 1 Null Null)) (Node 3 Null Null)
newTree=Node 26 (Node 10 (Node 4 Null Null) (Node 6 Null Null)) (Node 3 Null (Node 3 Null Null))
--Question No. 1 --check if it a sum tree=========================================================
checkTree Null=True
checkTree (Node n Null Null) = True
checkTree (Node a lt rt)=(a==sumTree lt+sumTree rt) && (checkTree lt && checkTree rt)
--Question No. 2 total===========================================================
sumLeafs Null=0
sumLeafs (Node a Null Null)=a
sumLeafs (Node a lt rt)=sumLeafs lt +sumLeafs rt
--Question No. 3 -- traverse in preorder===========================================================
preOrder Null=[]
preOrder (Node a lt rt)= a:preOrder lt ++ preOrder rt
--sum of tree---------------------------------------------------
sumTree Null=0
sumTree (Node a lt rt)=a+sumTree lt +sumTree rt
--count nodes of tree---------------------------------------------------
countTree Null=0
countTree (Node a lt rt)=1+countTree lt +countTree rt
--calc depth of tree----------------------------------------------
depthTree Null=0
depthTree (Node a lt rt)=1+max  (depthTree lt)  (depthTree rt)
--compare two trees (equivalent)----------------------------------------------
equivTree Null Null=False
equivTree tree1 tree2=sort(preOrder tree1)==sort(preOrder tree2)
--reflect tree (if left node is same to right node)----------------------------------------------
reflectTree Null=False
reflectTree (Node a lt rt)=preOrder lt==preOrder rt
--is balanced tree----------------------------------------------
isBalancedTree Null=False
isBalancedTree (Node a lt rt)=depthTree lt==depthTree rt
--Question No. 4 -- setIntersect===========================================================
intersects []=[]
intersects (x:[])=x
intersects (x1:x2:xs)=intersects $ (intersect x1 x2):xs
    where intersect [] _=[]
          intersect _ []=[]
          intersect (x:xs) ys=if elem x ys
                              then x:(intersect xs ys)
                              else (intersect xs ys)
intersects' []=[]
intersects' (x:[])=x
intersects' (x1:x2:xs)=intersects' $ (intersect x1 x2):xs
 
--a----------quickCheck on intersect
prop_intersectapp :: [[Int]] -> Bool
prop_intersectapp xs = intersects' xs == intersects xs

--main function --------------------------------------------------
main=do 
      print myTree
      print newTree
      print $ checkTree myTree
      print $ checkTree newTree
      print $ countTree newTree
      print $ sumTree newTree
      print $ sumLeafs newTree
      print $ depthTree newTree
      print $ preOrder newTree
      print $ equivTree myTree newTree
      print $ reflectTree newTree
      print $ isBalancedTree newTree
      print $ intersects [[1,2,3],[3,4,5],[1,3,4,7]]
      print $ intersects' [[1,2,3],[3,4,5],[1,3,4,7]]
      quickCheck prop_intersectapp

{-|  =================OUTPUT========================================================
Node 7 (Node 4 (Node 3 Null Null) (Node 1 Null Null)) (Node 3 Null Null)
Node 26 (Node 10 (Node 4 Null Null) (Node 6 Null Null)) (Node 3 Null (Node 3 Null Null))
False
True
6
52
13
3
[26,10,4,6,3,3]
False
False
True
[3]
[3]
(((+++ OK, passed 10 tests.
-}