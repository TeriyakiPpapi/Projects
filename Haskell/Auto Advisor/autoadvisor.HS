import system.IO
import Data.list 
import Control.Exception 
import Data.Array.Storable 
import "multiset" Data.MultiSet as X

autoAdvisor :: IO()

main = do 
     --read the file 
    let file = "csmajor" 
    contains <- openFile file ReadMode

    fileExist <- doesFileExist file 
     
    if not fileExist
    then putStrln "the system cannot find the file specified, try something else" 

	trimSpace :: string -> string 

    --try to see if arrays could work 
   -- try to store 

	arr <- tempPreqs
	tempPreqs <- newArray :: IO (StorableArray Int Int)


	let gpa = 0 :: Double 
	let attemptedHours = 0 :: Int 
	let completedHours = 0 :: Int 
	let remainingHours = 0 :: Int 

	--doing gpa calculation and completed and incompleted course 
	
	if X.fromListGrade /= 0 [and] hours /= 0
		
	let grade1 = A
		case of grade 1
		1 <- putStrln gpa (4 * hours) 
		2 <- putStrln completedHours
	
		let grade2 == B
			case of grade2 
			1 <- putStrLn gpa (3 * hours) 
			2 <- putStrLn completedHours
	
			let grade3 == C 
				case of grade3 
				1 <- putStrLn gpa (2 * hours) 
				2 <- putStrLn completedHours

					let grade4 == D
						case of grade4 
						1 <- putStrLn gpa (1 * hours)
						2 <- putStrln completedHours

						let grade5 == F
							case of grade5 
							1 <- putStrLn gpa (0 * hours )
							2 <- putStrLn compledtedHours
	 
		else
			putStrLn remainingHours

		---
	
-
	--
