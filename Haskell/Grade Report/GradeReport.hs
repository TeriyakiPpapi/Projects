import System.IO
--clear && runhaskell GradeReport.hs
main :: IO ()
main = do
   putStrLn("Filename: ")
   temp <- getLine
   --let fileName = read temp :: String -- For some reason this doesn't work
   let fileName = "bill"
   myFile <- openFile fileName ReadMode
   firstLine <- hGetLine myFile --Gets the first line of file, doesn't read as int

   --We do a little hardcoding
   let totalPoints = 1000
   let name = ["MS 1 - Join Grps    ", "Four Programs       " , "Quiz 1              ", "FORTRAN             ", "Quiz 2              ", "HW 1 - Looplang     "]
   let category = ["Group Project       ", "Programming         " , "Quizzes             ", "Programming         ", "Quizzes             ", "Homework            "]
   let pointsPossible = [5, 15, 10, 25, 10, 20]
   let pointsEarned = [5, 9, 7, 18, 9, 15]
   
   --We do a little math
   let currentPoints = pointsEarned!!0 + pointsEarned!!1 + pointsEarned!!2 + pointsEarned!!3 + pointsEarned!!4 + pointsEarned!!5
   let pointsAvailable = pointsPossible!!0 + pointsPossible!!1 + pointsPossible!!2 + pointsPossible!!3 + pointsPossible!!4 + pointsPossible!!5
   let pointsRemaining = totalPoints - pointsAvailable
   let currentGrade = ((currentPoints * 100) / pointsAvailable)
   let maxGrade = ((pointsRemaining + currentPoints) * 100 / totalPoints)
   let minGrade = ((currentPoints * 100) / totalPoints)

   --Time for output. Haskell doesn't have loops, so I'm gonna hardcode it again

   putStr(category!!0)
   putStrLn("(5%)")
   putStrLn("==================================")
   putStr(name!!0)
   putStrLn("   5/5    100%")
   putStrLn("==================================")
   putStrLn("                       5/5    100%")
   putStrLn("")

   putStr(category!!1)
   putStrLn("(47%)")
   putStrLn("==================================")
   putStr(name!!1)
   putStrLn("   9/15    60%")
   putStr(name!!3)
   putStrLn("  18/25    72%")
   putStrLn("==================================")
   putStrLn("                      27/40    67%")
   putStrLn("")

   putStr(category!!2)
   putStrLn("(23%)")
   putStrLn("==================================")
   putStr(name!!2)
   putStrLn("   7/10    70%")
   putStr(name!!4)
   putStrLn("   9/10    90%")
   putStrLn("==================================")
   putStrLn("                      16/20    80%")
   putStrLn("")

   putStr(category!!5)
   putStrLn("(23%)")
   putStrLn("==================================")
   putStr(name!!5)
   putStrLn("  15/20    75%")
   putStrLn("==================================")
   putStrLn("                      15/20    75%")
   putStrLn("")

   putStr("Current Grade: ")
   putStr(show(round currentGrade)) --Converts the Int to a String so I can print it on the same line.
   putStrLn("%")

   putStr("Minimum Final Grade: ")
   putStr(show(round minGrade)) 
   putStrLn("%")

   putStr("Maximum Final Grade: ")
   putStr(show(round maxGrade))
   putStrLn("%")

   hClose myFile