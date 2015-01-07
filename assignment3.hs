import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Char
data Major = Fr|So|Jr|Sr deriving (Show,Eq)
data Student = Student {
                name::String,
                age::Int,
                year::Int,
                major::Major
                }deriving (Show)
students=[Student "Ram" 33 2014 Fr,Student "Dinesh" 27 2014 Jr,Student "Shyam" 25 2014 Sr,Student "James" 25 2014 Sr]
searchStudent=[a|a<-students,major(a)==Sr]
-- end of question 1-----------------------------------------------------------------------------------
getDaysInMonth year month=(nDays,sDay) 
                          where nDays = gregorianMonthLength year month
                                sDay= digitToInt(last(showWeekDate (fromGregorian year month 01)))
yearTo=2013
month=10
months=["JANUARY","FEBURARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER"]
mnth=months!!(month-1)
monthDate = getDaysInMonth yearTo month
startAt=snd(monthDate)
totalDays = fst(monthDate)
printLine = "\n"++concat ["+---\t" | r <- [1..7]]
check x | x>totalDays=""
        | x<=totalDays=show x
genDays n m="\n "++concat[check(x)++"\t|"|x<-[n..m]]
genD 6=""
genD sP=genDays ((sP-1)*7+1) (sP*7) ++genD (sP+1)
generateCalendar=printLine++"\n "++
                        concat [[r]++"\t|" | r <- mnth]++printLine++
                        "\n Sun\t|Mon\t|Tue\t|Wed\t|Thu\t|Fri\t|Sat"++printLine++
                        genD 1
-- end of question 2-----------------------------------------------------------------------------------
main=do
     print searchStudent
     putStrLn generateCalendar
