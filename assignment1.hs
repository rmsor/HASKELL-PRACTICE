listSquares::Int->[[Int]]
triples::Int->[(Int,Int,Int)]
--average::[Int]->Int
splitOE::[Integer]->(Integer->Bool)->([Integer],[Integer])
x=["abc","efg"]
string="("++( x!!1!!2 : x!!1 ++ x!!0) ++ ")"
newList=[1,2,3] ++ [4,5,6]
[a,b] = [3,4]
listSquares n=[[a,a*a]|a<-[1..n]]
triples x=[(a,b,c)|a<-[1..x],b<-[1..x],c<-[1..x],a*a+b*b==c*c]
average x=sum x/fromIntegral (length x)
nums=[1,3,5,2,6,7,12,14,9]
splitOE xs preFunc=(a,b)
        where a=[x|x<-xs,preFunc x]
              b=[x|x<-xs,(not.preFunc) x]
main =do 
  print string 
  print newList
  print a
  print b
  print (listSquares 10)
  print (triples 20)
  print (average [1,2,3,4])
  print (splitOE nums even)