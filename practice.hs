type CustomerId=Int
type Name=String
type Authors=[String]
data BookInfo=Book CustomerId Name Authors
              deriving (Show)
myInfo=Book 809709809 "Lambda Calculus" ["Richard Bird","Oege Dell"]
sumList (x:xs)=x+sumList xs
sumList []=0
third(a,b,c)=c
complicated(True,a,x:xs,5)=(a,xs)

type CustomerID=Int
type Address=String
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
myCustomer=Customer 34342 "Ram" "Fairfield, Iowa"
myCustomer2=Customer{customerID=3423,customerAddress="Fairfield,Iowa",customerName="Dinesh"}

data Test a=Test a
          | Empty
          deriving (Show)
someBool=Test True
someString=Test "hello"
