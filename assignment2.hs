--Question Number 1 *****************
fact 0=1
fact n=n*fact(n-1)
choose m n=fact(m)/(fact(n)*fact(m-n))
--Question Number 2 *****************
choose1 m 0=1
choose1 m n= (m/n)* (choose1 (m-1) (n-1))
--Question Number 3 *****************
choose2 m 0=1
choose2 m n= (choose2 (m-1) n)+(choose2 (m-1) (n-1))
-- Question Number 5 -----------------------
--Second Approach seems to be best because it doesnot use much recursion to calculate the choose value. First one doesnont
--uses recursion to calculate choose value but uses recursion to calculate factorial multiple times.Third solution again uses recursion twice of 
--second approach
--Main Application *****************
main=do 
  print (choose 5 3)
  print (choose1 5 3)
  print (choose2 5 3)

