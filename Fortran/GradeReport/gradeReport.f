C***********************************Variables**********************************
      CHARACTER*32 Filename
      CHARACTER LINE(100)
      CHARACTER Temp(20)
      CHARACTER Temp2(20)
      INTEGER TotalPoints
      INTEGER currentPoints
      INTEGER PointsAvailable
      INTEGER PointsRemaining
      INTEGER CurrentGrade
      INTEGER MaxGrade
      INTEGER MinGrade
      INTEGER CategoryTotal
      INTEGER CurrrentCategory
      INTEGER CategoryWeight   
C************************************Arrays************************************
      CHARACTER Name(20,256)
      CHARACTER Category(20,256)
      INTEGER PointsPossible(14)
      INTEGER PointsEarned(14)
      CHARACTER BLANKARRAY

      CHARACTER BLANKCHAR
      INTEGER BLANKINT


10    FORMAT('File: ')
15    FORMAT(A16)

100   FORMAT('CurrentPoints: ', I14)
101   FORMAT('PointsAvailable: ', I14)
102   FORMAT('PointsRemaining: ', I14)
103   FORMAT('CurrentGrade: ', I14, '%')
104   FORMAT('MaxGrade: ', I14, '%')
105   FORMAT('MinGrade: ', I14, '%')

500   FORMAT()
510   FORMAT('Total Points: ', I14)
550   FORMAT(I14)

1000   FORMAT(100A)
1002  FORMAT(20A)
C**********************************Get Input***********************************
      WRITE(*, 10, ADVANCE="NO")      
      READ 15, FILENAME    
      OPEN(UNIT=22, FILE=FILENAME)

      READ(22, 550, IOSTAT=IOS) TotalPoints
C*********************************Store Data***********************************
      DO 2000 I=1,256
            READ(22, 1000, IOSTAT=IOS) LINE
            IF (IOS /= 0) EXIT
            DO 2100 J = 1, 20
                  Name(j, i) = LINE(J)
                  IF (IOS /= 0) GOTO 2000
2100        CONTINUE
            DO 2200 J = 1,20
                  Category(J, I) = LINE(J+20)
2200        CONTINUE

C Find a way to put the last part into the char arrays
C All attempts have failed :(

2000  CONTINUE
      CLOSE(UNIT=22)

      PointsPossible(1) = 5
      PointsPossible(2) = 15
      PointsPossible(3) = 10
      PointsPossible(4) = 25
      PointsPossible(5) = 10
      PointsPossible(6) = 20
      PointsEarned(1) = 5
      PointsEarned(2) = 9
      PointsEarned(3) = 7
      PointsEarned(4) = 18
      PointsEarned(5) = 9
      PointsEarned(6) = 15      

c      DO 3000 I= 1,256
c            IF (NAME(1, I) .EQ. BLANKCHAR) EXIT
c            WRITE(*, 1002) Name(:,I)
c            WRITE(*, 1002) Category(:,I)
c            WRITE(*, 550) PointsPossible(I)
c            WRITE(*, 550) PointsEarned(I)
c            WRITE(*, 500)
c3000  CONTINUE
C**********************************Calculate***********************************
      currentPoints = 0
      DO 106 i = 1, 256
            IF (PointsEarned(i+1) .EQ. BLANKINT) GOTO 88
            currentPoints = currentPoints + PointsEarned(i)
106   CONTINUE
88    currentPoints = currentPoints + PointsEarned(i)

      PointsAvailable = 0
      DO 20 i = 1, 256
            IF (PointsPossible(i+1) .EQ. BLANKINT) GOTO 89
            PointsAvailable = PointsAvailable + PointsPossible(i)
20    CONTINUE
89    PointsAvailable = PointsAvailable + PointsPossible(i)

      PointsRemaining = TotalPoints - PointsAvailable
      CurrentGrade = ((currentPoints * 100) / PointsAvailable)
      MaxGrade = ((PointsRemaining + currentPoints) * 100 / TotalPoints)
      MinGrade = ((currentPoints * 100) / TotalPoints)

C***********************************Output*************************************
C Section won't work as I don't have a clear way to compare strings. 
      DO 10000 I = 1, 256
            Temp = Category(:,I)
c            IF (Temp .EQ. BLANKARRAY) GOTO 10000
            CategoryTotal = 0
            CurrrentCategory = 0
            CategoryWeight = 0
10000 CONTINUE
C I don't know why, but not having this here breaks the code. Don't ask me
      WRITE(*, 101) PointsAvailable

      WRITE(*, 103) CurrentGrade
      WRITE(*, 104) MaxGrade
      WRITE(*, 105) MinGrade
      END
