C Auto Advisor
C Author: Nathan Kiehl

C ---------- How to open a file and regurgitate contents line by line ----------
C      OPEN(UNIT=22, FILE=FILENAME)
C      
C      DO 2000 I=1,500
C      READ(22, 650, IOSTAT=IOS) LINE
C      IF (IOS /= 0) GOTO 2000
C      WRITE(*, 650) LINE
C2000  CONTINUE
C      CLOSE(UNIT=22)

C ------------------------------------------------
C Arrays
C ------------------------------------------------

      CHARACTER CNAMES(32,256)
      INTEGER   HOURS(256)
C     (characters, 'and' courses, 'or' sections, total courses)
C     (PREQCHAR, PREQ, SECT, I)
      CHARACTER PREQS(32,8,8,256)
      CHARACTER GRADE(256)
      
C ---------------- End of Arrays -----------------
C Variables
C ------------------------------------------------
      
C --------------- Actual Variables ---------------
C     GPA
      REAL    GPA
C     Hours Attempted
      INTEGER HRA
C     Hours Completed
      INTEGER HRC
C     Hours Remaining
      INTEGER HRR
C     Character on line
      INTEGER CRN
      
C     Total Courses
      INTEGER TOTAL/0/
C     Maximum 'and' categories
      INTEGER TOTAND/0/
C     Maximum 'or' categories
      INTEGER TOTOR/0/
      
C Temporary character stores for number arrays
C                 Hours String
      CHARACTER HRSSTR(3)
      
C Loop variables for prerequisites
      INTEGER SECT
      INTEGER PREQ
      INTEGER PREQCHAR
      
C Store and deal with strings      
      CHARACTER*32 FILENAME
      CHARACTER LINE(64)
!      CHARACTER, DIMENSION(64) :: LINE

C Boolean to print log files or not
      LOGICAL PRNLOG/.FALSE./
      LOGICAL PRNLOG2/.FALSE./
      
C Variables for printing part
      LOGICAL PREQLEFT/.FALSE./
      LOGICAL CANTAKE
      CHARACTER NOCHAR

C -------------- End of Variables ----------------
C Formatters
C ------------------------------------------------

10    FORMAT('File: ')
15    FORMAT(A16)
20    FORMAT('GPA: ', F6.2)
30    FORMAT('Hours Attempted: ', I3)
40    FORMAT('Hours Completed: ', I3)
50    FORMAT('Credits Remaining: ', I3)

100   FORMAT('Possible Courses to Take Next')
110   FORMAT(2X, 32A)

500   FORMAT()
600   FORMAT(A64)

610   FORMAT('Name: ', 32A)
620   FORMAT('Hours: ', I3)
630   FORMAT('Preq: ', 32A)
640   FORMAT('Grade: ', A)

650   FORMAT(64A)
700   FORMAT(3I1)

C Formats to check printing logic
810   FORMAT('COURSE ITERATION: ', I3, ' ', 32A, ' GRADE ', 1A)
820   FORMAT('OR ITERATION: ', I1)
830   FORMAT('AND ITERATION: ', I1)
840   FORMAT('COMPARING ' 32A, ' AND ', 32A)
850   FORMAT('CANTAKE = FALSE')

C -------------- End of Formatters ---------------
C Accept Input
C ------------------------------------------------

C Note: ADVANCE="NO" IS NOT FORTRAN 4 CODE
      WRITE(*, 10)
      READ 15, FILENAME
!      filename = 'sophomore'
      
C PAGE 84/87
      OPEN(UNIT=64, FILE=FILENAME)
      
C Go through whole file line by line
      DO 1000 I=1,256
      SECT     = 1
      PREQ     = 1
      PREQCHAR = 1
      CRN      = 1
      IF (PRNLOG) WRITE(*, 500)
      
      READ(64, 650, IOSTAT=IOS) LINE
      IF (IOS /= 0) EXIT
      IF (PRNLOG) WRITE(*, 650) LINE
      TOTAL = TOTAL + 1
      
C Read a name up to 32 characters long
2100  DO 1100 J=1,32
      IF (LINE(CRN) .EQ. '|') GOTO 2200
      CNAMES(J,I) = LINE(CRN)
      CRN = CRN + 1
1100  CONTINUE

      

C Read up to 3 digits for credit amount
2200  CRN = CRN + 1
      IF (PRNLOG) WRITE(*, 610) CNAMES(:,I)
      DO 1200 J=1,3
      IF (LINE(CRN) .EQ. '|') GOTO 2300
      
      HRSSTR(J) = LINE(CRN)
      READ(HRSSTR, 700) HOURS(I)
      CRN = CRN + 1
      
1200  CONTINUE

C Read pre-requisites
2300  IF (PRNLOG) WRITE(*, 620) HOURS(I)
      DO 1300 J=1,64
      CRN = CRN + 1
      
      IF (LINE(CRN) .EQ. '|') GOTO 2600
      IF (LINE(CRN) .EQ. ' ') GOTO 2500
      IF (LINE(CRN) .EQ. ',') GOTO 2400
      
      IF (TOTOR .LT. SECT) TOTOR = SECT
      IF (TOTAND .LT. PREQ) TOTAND = PREQ
      
      PREQS(PREQCHAR, PREQ, SECT, I) = LINE(CRN)
      PREQCHAR = PREQCHAR + 1
      GOTO 1300

C And group
2400  IF (PRNLOG) WRITE(*, 630) PREQS(:,PREQ,SECT,I)
      PREQ = PREQ + 1
      PREQCHAR = 1
      
      GOTO 1300

C Or group
2500  IF(   (PREQS(1,PREQ,SECT,I) .NE. 'S')
     &.AND. (PREQS(2,PREQ,SECT,I) .NE. 'e')
     &.AND. (PREQS(3,PREQ,SECT,I) .NE. 'n')
     &.AND. (PREQS(4,PREQ,SECT,I) .NE. 'i')
     &.AND. (PREQS(5,PREQ,SECT,I) .NE. 'o')
     &.AND. (PREQS(6,PREQ,SECT,I) .NE. 'r')) GOTO 2550
      
      PREQS(7,PREQ,SECT,I)  = ' '
      PREQS(8,PREQ,SECT,I)  = 'S'
      PREQS(9,PREQ,SECT,I)  = 't'
      PREQS(10,PREQ,SECT,I) = 'a'
      PREQS(11,PREQ,SECT,I) = 'n'
      PREQS(12,PREQ,SECT,I) = 'd'
      PREQS(13,PREQ,SECT,I) = 'i'
      PREQS(14,PREQ,SECT,I) = 'n'
      PREQS(15,PREQ,SECT,I) = 'g'
      IF (PRNLOG) WRITE(*, 630) PREQS(:,PREQ,SECT,I)
      CRN = CRN + 8
      PREQ = PREQ + 1
      PREQCHAR = 1
      GOTO 1300
      
C Finish 'or' logic
2550  IF (PRNLOG) WRITE(*, 630) PREQS(:,PREQ,SECT,I)
      SECT = SECT + 1
      PREQ = 1
      PREQCHAR = 1

C Finish 'and' logic
1300  CONTINUE

C Add grade to list
2600  CRN = CRN + 1
      IF (PRNLOG) WRITE(*, 630) PREQS(:,PREQ,SECT,I)
      IF (LINE(CRN) .EQ. '') GOTO 1001
      GRADE(I) = LINE(CRN)
      
      IF (GRADE(I) .EQ. 'A') GOTO 490
      IF (GRADE(I) .EQ. 'B') GOTO 480
      IF (GRADE(I) .EQ. 'C') GOTO 470
      IF (GRADE(I) .EQ. 'D') GOTO 460
      IF (GRADE(I) .EQ. 'F') GOTO 450
      
490   GPA = GPA + 4*HOURS(I)
      HRC = HRC + HOURS(I)
      GOTO 400
480   GPA = GPA + 3*HOURS(I)
      HRC = HRC + HOURS(I)
      GOTO 400
470   GPA = GPA + 2*HOURS(I)
      HRC = HRC + HOURS(I)
      GOTO 400
460   GPA = GPA + 1*HOURS(I)
      HRR = HRR + HOURS(I)
      GOTO 400
450   HRR = HRR + HOURS(I)
      
      
400   HRA = HRA + HOURS(I)
      IF (PRNLOG) WRITE(*, 640) GRADE(I)
      GOTO 1000
      
C     End of line
1001  HRR = HRR + HOURS(I)
1000  CONTINUE

      CLOSE(64)
C ------------- End of Accept Input --------------
C Print Results
C ------------------------------------------------
     
      IF (GPA .NE. 0 .AND. HRA .NE. 0) GOTO 3000
      GPA = 0
      GOTO 3100
3000  GPA = GPA / HRA

3100  WRITE(*, 20) GPA
      WRITE(*, 30) HRA
      WRITE(*, 40) HRC
      WRITE(*, 50) HRR
      WRITE(*, 500)
      
      WRITE(*, 100)
     
C     This is where I'll write the part of the program that
C     determines a course's takeability and print it
      
      DO 10000 I=1,TOTAL
      IF (PRNLOG2) WRITE(*, 810) I, CNAMES(:,I), GRADE(I)
      IF (
     &GRADE(I) .EQ. 'A' .OR.
     &GRADE(I) .EQ. 'B' .OR.
     &GRADE(I) .EQ. 'C' .OR.
     &GRADE(I) .EQ. 'D') GOTO 10000
      
C     If a grade is too low or no grade is present, will be false and course won't be taken
      CANTAKE = .TRUE.
C     Check 'or' groups
      DO 11000 J=1,TOTOR
      IF (PREQS(1,1,J,I) .EQ. NOCHAR) GOTO 10100
      IF (PRNLOG2) WRITE(*, 820) J
      CANTAKE = .TRUE.
      
C     Check 'and' groups
      DO 12000 K=1,TOTAND
      IF (PREQS(1,K,J,I) .EQ. NOCHAR) GOTO 11000
      IF (.NOT. CANTAKE) GOTO 11000
      IF (PRNLOG2) WRITE(*, 830) K
      
C     Check every course
      DO 12500 L=1,TOTAL
      IF (PRNLOG2) WRITE(*, 840) PREQS(:,K,J,I), CNAMES(:,L)
      
C     Compare preqs to courses list letter by letter
      DO 12550 M=1,32
      IF (PREQS(M,K,J,I) .NE. CNAMES(M,L)
     &)GOTO 12500
12550 CONTINUE

C     Only count course if it has been passed
      IF (GRADE(L) .NE. 'A' .AND.
     &GRADE(L) .NE. 'B' .AND.
     &GRADE(L) .NE. 'C')
     &CANTAKE = .FALSE.
!      IF (PRNLOG2) WRITE(*, 850)
!      ENDIF
      GOTO 12000

C     End every course
12500 CONTINUE
      
      CANTAKE = .FALSE.
C     End 'and'
12000 CONTINUE

      IF (CANTAKE) GOTO 10100
C     End 'or'
11000 CONTINUE

C     
10100 IF (.NOT. CANTAKE) GOTO 10000
      PREQLEFT = .TRUE.
      WRITE(*, 110) CNAMES(:,I)
10000 CONTINUE
      
C ------------- End of Print Results -------------
      END
