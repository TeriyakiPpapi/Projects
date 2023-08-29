C This is the quicksor code in Fortran IV written by Nathan Kiehl
C Java logic reference created by Reece Wehmeyer
C --------------------------------------------------------------------- 

C Make an array to store user input
      INTEGER M(10)

C Ask user for input      
      WRITE(*, 1)
1     FORMAT("Enter 10 Integers")

C Loop 10 times, get an integer from the user and store it in the array      
      DO 3 I = 1, 10

      READ(*, 2) M(I)
2     FORMAT(I3)
3     CONTINUE

C Perform QS on the array
C Page 75 in pdf makes way for recursion without using recursion
      CALL QS(M, 1, 10, QS)
      
      WRITE(*, 4)
4     FORMAT()
      
      WRITE(*, 5)
5     FORMAT('Sorted')
      
C Print Results
      DO 6 I=1,10
      WRITE(*, 2) M(I)
6     CONTINUE
      END
C ---------------------------------------------------------------------

C QuickSort algorithm not technically using recursion
C Takes array of integers, the index of first value, and the index of
C the last value
      SUBROUTINE QS(M, F, L, DUMSUB)

C Declare variables
      INTEGER M(10)
      INTEGER F
      INTEGER L
      INTEGER PARTIND
C The device to allow for psuedo recursion
      EXTERNAL DUMBSUB
C run quicksort again if start is less than end (indexes)
      IF (F .GE. L) GOTO 100
      
C partition index is the value returned by running partition function
      CALL PART(M, F, L, PARTIND)
      
C run quicksort on top half and bottom half of array
      CALL DUMSUB(M, F, PARTIND-1, DUMSUB)
      CALL DUMSUB(M, PARTIND+1, L, DUMSUB)
      
100   END
      
      
      
      
C partition algorithm used by quickSort
C takes an array of integers, the index of the starting value, and the index of the ending value
      SUBROUTINE PART(M, F, L, PARTIND)
      
C Declare variables
      INTEGER M(10)
      INTEGER F
      INTEGER L
      INTEGER PARTIND
      INTEGER PIVOT
      INTEGER I
      INTEGER T
      
C the pivot is the last value in the array      
      PIVOT = M(L)
C set i to be 1 less than the starting index
      I = F-1
      
C loop through every value in the array
      DO 200 J=F,L-1
      
C if the value at index j is smaller than the pivot, swap with index i
      IF (M(J) .GT. PIVOT) GOTO 200
      
C increment i to keep track of our indexes
      I = I + 1
      
C swap the values of indexes i and j to move smaller values below the pivot
      T = M(I)
      M(I) = M(J)
      M(J) = T
      
200   CONTINUE

c do final swap to put pivot in its proper index
      T = M(I + 1)
      M(I + 1) = M(L)
      M(L) = T
      
C return index of pivot
      PARTIND = I + 1

      END
C ---------------------------------------------------------------------
