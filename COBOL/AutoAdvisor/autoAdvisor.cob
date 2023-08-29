      * Generate a list of possible courses based on a transcript
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      Auto_Advisor.
       AUTHOR.          Nathan Kiehl.
       DATE-WRITTEN.    9/30/21.
       
      *---------------------------------------------------- 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT infile ASSIGN TO filename
            ORGANIZATION IS LINE SEQUENTIAL.
            
      *----------------------------------------------------       
       DATA DIVISION.
       FILE SECTION.
       FD infile.
       01 infile-file.
            05 none PIC X(256).
       
       WORKING-STORAGE SECTION.
      * Main data sets, houses all imported information.
       01 courses-TABLE OCCURS 256 TIMES.
            10 readline PIC X(256).
            10 name PIC X(32).
            10 hours PIC 9(3).
                  
            10 preqs PIC X(220).
            10 preq-ors OCCURS 8 TIMES.
                  15 preq-ord PIC X(64).
                  15 preq-ands OCCURS 8 TIMES.
                        20 preq-anded PIC X(64).
                  
            10 grade PIC A(1).
       
      * Other variables
       01 gpa           PIC 9(1)V99.
       01 gpaSum        PIC 9(3).
       01 attHours      PIC 9(3).
       01 compHours     PIC 9(3).
       01 remHours      PIC 9(3).
       
       01 zattHours     PIC Z(3)9.
       01 zcompHours    PIC Z(3)9.
       01 zremHours     PIC Z(3)9.
       
       01 temp          PIC 9(2).
       01 sen-cnt       PIC 9(1).
            
       01 EOF           PIC A(1).
       
       01 course        PIC 9(3).
       01 preq-orn      PIC 9(1).
       01 preq-andn     PIC 9(1).
       
       01 disp1         PIC 9(3).
       01 disp2         PIC 9(1).
       01 disp3         PIC 9(1).
       01 disp4         PIC 9(3).
       
       01 preq-left     PIC 9(1).
       01 can-take      PIC 9(1).
            
      *----------------------------------------------------  
       PROCEDURE DIVISION.
       MOVE 'sophomore' TO filename.
       DISPLAY 'File: ' FUNCTION TRIM(filename).
      *ACCEPT filename.
       OPEN INPUT infile.
       
       SET course TO 1.
       SET gpaSum to 0.
       PERFORM PARSE-COURSE UNTIL EOF='Y'.
      *PERFORM PARSE-COURSE 5 TIMES.
       
       DONE-PARSE.
       CLOSE infile.
       
       IF gpaSum IS EQUAL 0 OR attHours IS EQUAL 0
            THEN SET gpa to 0
            ELSE COMPUTE gpa = gpaSum / attHours.
       
       MOVE attHours TO zattHours.
       MOVE compHours to zcompHours.
       MOVE remHours TO zremHours
            
       DISPLAY 'GPA: ' gpa.
       DISPLAY 'Hours Attempted: ' FUNCTION TRIM(zattHours).
       DISPLAY 'Hours Completed: ' FUNCTION TRIM(zcompHours).
       DISPLAY 'Credits Remaining: ' FUNCTION TRIM(zremHours).
       DISPLAY ' '.
       DISPLAY 'Possible Courses to Take Next'.
       
       SET preq-left TO 0.
       SET disp1 TO 1.
       PERFORM LIST-COURSES UNTIL disp1 EQUALS course.
       
       IF preq-left EQUALS 0 DISPLAY '  None - Congratulations!'.
       
       STOP RUN.
       
       
       PARSE-COURSE.
            READ infile INTO readline(course)
                  AT END MOVE 'Y' TO EOF
            END-READ.
            
      *     MOVE 'CS591|3|CS300,CS380,CS480 CS300,CS503 CS500,CS503|A'
      *     TO readline(course)(.
            
            IF EOF='Y' GO TO DONE-PARSE.
            
      *     DISPLAY FUNCTION TRIM(readline(course)).
            
            UNSTRING FUNCTION TRIM(readline(course))
                 DELIMITED BY '|'
                 INTO  name(course)
                       hours(course)
                       preqs(course)
                       grade(course)
            END-UNSTRING.
            
            IF grade(course) IS NOT EQUAL ' '
            AND hours(course) IS NOT EQUAL ' '
                  THEN PERFORM GRADE-SUM
                  ELSE ADD hours(course) to remHours.
            
      *     DISPLAY 'name(' course ')       : ' name(course).
      *     DISPLAY 'hours(' course ')      : ' hours(course).
      *DISPLAY 'preqs(' course ')      :'
      *FUNCTION TRIM(preqs(course)).
      *    DISPLAY 'grade(' course ')      : ' grade(course).
                  
            UNSTRING preqs(course)
                  DELIMITED BY ' '
                  INTO  preq-ord(course, 1)
                        preq-ord(course, 2)
                        preq-ord(course, 3)
                        preq-ord(course, 4)
                        preq-ord(course, 5)
                        preq-ord(course, 6)
                        preq-ord(course, 7)
                        preq-ord(course, 8)
             END-UNSTRING.
             
             SET preq-orn TO 1
             PERFORM PARSE-AND-PARA 8 TIMES.
             ADD 1 TO course.
      * End PARSE-COURSE-PARA.
       
       PARSE-AND-PARA.
             UNSTRING preq-ord(course, preq-orn)
                  DELIMITED BY ','
                  INTO  preq-anded(course,preq-orn,1)
                        preq-anded(course,preq-orn,2)
                        preq-anded(course,preq-orn,3)
                        preq-anded(course,preq-orn,4)
                        preq-anded(course,preq-orn,5)
                        preq-anded(course,preq-orn,6)
                        preq-anded(course,preq-orn,7)
                        preq-anded(course,preq-orn,8)
            END-UNSTRING.
            SET sen-cnt TO 1.
            PERFORM CHECK-SENIOR 7 TIMES.
            ADD 1 TO preq-orn.
      * End PARSE-AND-PARA.
      
       CHECK-SENIOR.
            IF FUNCTION TRIM(preq-anded(course,preq-orn,sen-cnt))
            EQUALS 'Senior' THEN
                  MOVE 'Senior Standing'
                  TO preq-anded(course,preq-orn,sen-cnt).
            IF FUNCTION TRIM(preq-anded(course,preq-orn,sen-cnt))
            EQUALS 'Standing' THEN
                  MOVE ' '
                  TO preq-anded(course,preq-orn,sen-cnt).
      
       GRADE-SUM.
            IF grade(course) IS EQUAL TO 'A'
                  SET temp TO hours(course)
                  MULTIPLY 4 BY temp
                  ADD temp TO gpaSum
                  ADD hours(course) to compHours.
            IF grade(course) IS EQUAL TO 'B'
                  SET temp TO hours(course)
                  MULTIPLY 3 BY temp
                  ADD temp TO gpaSum
                  ADD hours(course) to compHours.
            IF grade(course) IS EQUAL TO 'C'
                  SET temp TO hours(course)
                  MULTIPLY 2 BY temp
                  ADD temp TO gpaSum
                  ADD hours(course) to compHours.
            IF grade(course) IS EQUAL TO 'D'
                  SET temp TO hours(course)
                  MULTIPLY 1 BY temp
                  ADD temp TO gpaSum
                  ADD hours(course) to remHours.
            IF grade(course) IS EQUAL TO 'F'
                  SET temp TO hours(course)
                  MULTIPLY 0 BY temp
                  ADD temp TO gpaSum
                  ADD hours(course) to remHours.
            
            ADD hours(course) to attHours.
       
      * End GRADE-SUM.
      
      
      *SET preq-left TO 0.
      *SET disp1 TO 1.
      *PERFORM LIST-COURSES UNTIL disp1 EQUALS course.
       LIST-COURSES.
       IF grade(disp1) EQUALS ' ' OR
          grade(disp1) EQUALS 'D' OR
          grade(disp1) EQUALS 'F'
            SET can-take TO 1
            SET disp2 TO 1
            PERFORM UNTIL disp2 EQUALS 9
                  SET can-take TO 1
                  SET disp3 TO 1
                  PERFORM UNTIL disp3 EQUALS 9
                        SET disp4 TO 1
                        PERFORM UNTIL disp4 EQUALS course
                              IF
                              FUNCTION TRIM(name(disp4)) EQUALS
                              FUNCTION TRIM(preq-anded
                              (disp1,disp2,disp3))
                                    IF grade(disp4) EQUALS 'A' OR
                                       grade(disp4) EQUALS 'B' OR
                                       grade(disp4) EQUALS 'C'
                                          EXIT PERFORM
                                    ELSE
                                    IF grade(disp4) EQUALS 'D' OR
                                       grade(disp4) EQUALS 'F' OR
                                       grade(disp4) EQUALS ' '
                                          SET can-take TO 0
                                          EXIT PERFORM
                                    END-IF
                              ELSE IF FUNCTION TRIM(preq-anded
                              (disp1,disp2,disp3))
                              NOT EQUALS ' '
                                    SET can-take TO 0
                              
                              END-IF
                              ADD 1 TO disp4
      *                       DISPLAY '(' disp1 ', ' disp2 ','
      *                       disp3 ', ' disp4 ')'
                        END-PERFORM
                        ADD 1 TO disp3
                        IF can-take EQUALS 1 EXIT PERFORM END-IF
                  END-PERFORM
                  ADD 1 TO disp2
                  IF can-take EQUALS 1 EXIT PERFORM END-IF
            END-PERFORM
            IF can-take EQUALS 1
                  SET preq-left TO 1
                  DISPLAY '  ' FUNCTION TRIM(name(disp1))
            END-IF
            
       END-IF
       ADD 1 TO disp1.

