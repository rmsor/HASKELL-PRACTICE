import Data.List
type Coin=Int
type Change=[(Int,Coin)]
type Solns=[Change]

--coins::Int->[Coin]->Solns
coins n []=[]
coins n _|n<0=[]
coins n (x:xs)=(div n x,x): (coins(mod n x) xs)

coins' 0 _=[[]]
coins' _ []=[]
coins' n (c:cs)=[(m,c):css|m<-[0..nc],css<-coins'(n-m*c) cs] 
                where nc=div n c


getValidComb []=[]
getValidComb (x:xs)= getValidList x:getValidComb xs
            where getValidList []=[]
                  getValidList (x:xs)=if fst(x)>0 then x:getValidList xs
                                      else getValidList xs

getSumComb []=[]
getSumComb (x:xs)= getSum x:getSumComb xs
            where getSum []=0
                  getSum (x:xs)=fst(x)+getSum xs

possibleComb=coins' 10 [20,10,5,4,1]
validComb=getValidComb possibleComb
coinsInComb=getSumComb validComb
bestComb=(validComb)!!(head $ elemIndices (minimum coinsInComb) coinsInComb)

main=do 
      putStrLn "\n-----------------Possible Combinations-----------"
      print possibleComb
      putStrLn "\n-----------------After Removing Zeros-----------"
      print validComb
      putStrLn "\n-----------------Get Number of coins in each combination-----------"
      print coinsInComb
      putStrLn "\n----- Best Combination is (first of the least coins) ---------------------"
      print bestComb

--a          OUTPUT
--a-----------------Possible Combinations-----------
--a[[(0,20),(0,10),(0,5),(0,4),(10,1)],[(0,20),(0,10),(0,5),(1,4),(6,1)],[(0,20),(0,10),(0,5),(2,4),(2,1)],[(0,20),(0,10),(1,5),(0,4),(5,1)],[(0,20),(0,10),(1,5),(1,4),(1,1)],[(0,20),(0,10),(2,5)],[(0,20),(1,10)]]
--a
--a-----------------After Removing Zeros-----------
--a[[(10,1)],[(1,4),(6,1)],[(2,4),(2,1)],[(1,5),(5,1)],[(1,5),(1,4),(1,1)],[(2,5)],[(1,10)]]
--a
--a-----------------Get Number of coins in each combination-----------
--a[10,7,4,6,3,2,1]
--a
--a----- Best Combination is (first of the least coins) ---------------------
--a[(1,10)]
