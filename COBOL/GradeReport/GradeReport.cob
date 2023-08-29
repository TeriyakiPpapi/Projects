       IDENTIFICATION DIVISION.
       PROGRAM-ID. GradeReport.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILENAME ASSIGN TO fileString
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD FILENAME.
       01 FILENAME-FILE.
           05 AssignmentName PIC A(20).
           05 Category PIC A(20).
           05 PointsPossible PIC 9(14).
           05 PointsEarned PIC 9(14).

       WORKING-STORAGE SECTION.

       01 totalPoints PIC 9(14).
       01 currentPoints PIC 9(14).
       01 pointsAvailable PIC 9(14).
       01 pointsRemaining PIC 9(14).
       01 currentGrade PIC 9(14).
       01 maxGrade PIC 9(14).
       01 minGrade PIC 9(14).
       01 categoryTotal PIC 9(14).
       01 categoryCurrent PIC 9(14).
       01 cWeight PIC 9(14).
       01 tempNum PIC Z(14)9.
       01 tempNum2 PIC Z(14)9.
       01 tempNum3 PIC Z(14)9.

       01 fileString PIC A(20).
       01 tempString PIC A(20) VALUE ' '. 
       01 emptyString PIC A(20) VALUE ' '.
      *Set the table to have X rows. not sure how to get this info, so make arbitrarily large
       01 fileArray.
           05 tableRow OCCURS 10 TIMES. 
              10 asgName PIC X(20).
              10 cat PIC X(20).
              10 pP PIC 9(14).
              10 pE PIC 9(14).
       01 EOF PIC A(1). 

       01 Counter PIC 9(10).
       01 Counter2 PIC 9(10).

       PROCEDURE DIVISION.
      *This will fill the TableArray with all the lines of the data 
           SET Counter TO 1.

           DISPLAY 'File: '.
           ACCEPT fileString.
           OPEN INPUT FILENAME.

           READ FILENAME NEXT RECORD INTO TotalPoints 

           PERFORM UNTIL EOF='Y'
                READ FILENAME INTO TableRow(Counter)
                   AT END MOVE 'Y' TO EOF
                END-READ
                ADD 1 TO Counter
                
           END-PERFORM.
           CLOSE FILENAME.
      *Next, I need to delete all the blank indexes of the table/array

      *Then, I'll calculate a couple things for our future math
           
           SET Counter TO 1.
           SET currentPoints TO 0.
           PERFORM UNTIL Counter=10
              ADD FUNCTION NUMVAL(pE(Counter)) TO currentPoints
              ADD 1 TO Counter
           END-PERFORM.

           SET Counter TO 1.
           SET pointsAvailable TO 0.
           PERFORM UNTIL Counter=10
              ADD FUNCTION NUMVAL(pP(Counter))TO pointsAvailable 
              ADD 1 TO Counter
           END-PERFORM.

           SET pointsRemaining TO 0.
           COMPUTE pointsRemaining = totalPoints - pointsAvailable

           SET currentGrade TO 0.
           COMPUTE currentGrade = currentPoints * 100 / pointsAvailable

           SET maxGrade TO 0.
           COMPUTE maxGrade = (pointsRemaining + currentPoints)*100
           COMPUTE maxGrade = maxGrade / totalPoints  

           SET minGrade TO 0.
           COMPUTE minGrade = ((currentPoints * 100) / totalPoints)

      *Finally, I format and print the output
           DISPLAY " "
           SET Counter to 1.
           PERFORM UNTIL Counter = 10
           IF cat(Counter) IS NOT EQUAL " " THEN
              COMPUTE cWeight = FUNCTION NUMVAL(pE(Counter))* 100
              COMPUTE cWeight = cWeight / FUNCTION NUMVAL(pP(Counter))
              MOVE cWeight to tempNum 
              DISPLAY cat(Counter) FUNCTION TRIM(tempNum) "%" 
              DISPLAY "================================="

              MOVE pE(Counter) to tempNum
              MOVE pP(Counter) to tempNum2
              MOVE cWeight to tempNum3
              DISPLAY asgName(Counter) FUNCTION TRIM(tempNum)"/" 
              FUNCTION TRIM(tempNum2) "    " FUNCTION TRIM(tempNum3) "%"
              DISPLAY "================================="

              DISPLAY "                      " FUNCTION TRIM(tempNum)"/" 
              FUNCTION TRIM(tempNum2) "    " FUNCTION TRIM(tempNum3) "%"
              DISPLAY " "
              SET cWeight to 0

           END-IF
           ADD 1 TO Counter
           END-PERFORM

           MOVE currentGrade TO tempNum
           DISPLAY "Current Grade: " FUNCTION TRIM(tempNum)  "%"
           MOVE minGrade TO tempNum
           DISPLAY "Minimum Final Grade: " FUNCTION TRIM(tempNum)"%"
           MOVE maxGrade TO tempNum
           DISPLAY "Maximum Final Grade: " FUNCTION TRIM(tempNum)"%"

           STOP RUN.
