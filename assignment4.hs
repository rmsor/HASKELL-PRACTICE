-- mydrop using recursion
myDrop _ []=[]
myDrop 0 xs=xs
myDrop n (x:xs)=myDrop(n-1) xs

-- mytake using recursion
myTake 0 _=[]
myTake _ []=[]
myTake n (x:xs)=x:myTake(n-1) xs

-- mytakewhile using recursion
myTakewhile _ []=[]
myTakeWhile p (x:xs)=if (p x ==True) then
                        x:myTakeWhile p xs
                      else
                      []
-- mydropwhile using recursion
myDropWhile _ []=[]
myDropWhile p (x:xs)=if (p x ==False) then
                        x:xs
                      else
                        myDropWhile p xs
-- lenght using fold
myLength xs=foldl (+) 0 [1|x<-xs]
-- lenght using recursion
myLength' []=0
myLength' (x:xs)=1+myLength'(xs)

-- concat using recursion
myConcat [[]]=[]
myConcat []=[]
myConcat (x:xs)=x++myConcat(xs)

--concat using fold
myConcat' xs=foldl (++) [] xs

main=do
      print $ myDrop 2 [1,2,3,4,5]
      print $ myTake 3 [1,2,3,4,5]
      print $ myDropWhile (<3) [1,2,3,4,5]
      print $ myTakeWhile (<3) [1,2,3,4,5]
      print $ myLength [1,2,3,4,5]
      print $ myLength' [1,2,3,4,5]
      print $ myConcat [[1,2],[3,4,5],[6,7]]
      print $ myConcat' [[1,2],[3,4,5],[6,7]]

  
