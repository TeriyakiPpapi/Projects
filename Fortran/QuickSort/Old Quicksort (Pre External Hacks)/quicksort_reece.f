C This is the quicksort code in Fortran IV written by Reece Wehmeyer
C --------------------------------------------------------------------- 

C Make an array to store user input
      DIMENSION M(10, 1)

C Make an int P to hold pivot point for QS and T as a temp
      INTEGER P
      INTEGER T

C Ask user for input      
      WRITE(*, 1)
1     FORMAT("Enter 10 Integers")

C Loop 10 times, get an integer from the user and store it in the array      
      DO 3 I = 1, 10

      READ(*, 2) NUM
2     FORMAT(I3)
      M(I, 1) = NUM

3     CONTINUE


C Perform QS on the array      
      P = QS(M, 1, 10)

C Loop until the entire array is sorted
C Sort bottom half of array
      T = P - 1
4     IF(T .LE. 1) GOTO 5
      

      T = QS(M, 1, T)
      GOTO 4


C Sort top half of array
5     T = P + 1
6     IF(10 .GE. T) GOTO 7


      T = QS(M, T, 10)
      GOTO 6


C Print results
7     WRITE(*, 8)
8     FORMAT("Sorted")

C Loop 10 times for printing
      DO 10 I = 1, 10

      WRITE(*, 9) M(I, 1)
9     FORMAT(I3)

10    CONTINUE

      END
C ---------------------------------------------------------------------

C QuickSort algorithm not technically using recursion
C Takes array of integers, the index of first value, and the index of
C the last value
      REAL FUNCTION QS(M, F, L)

      INTEGER F
      INTEGER L
      INTEGER T
      INTEGER I
      REAL P
      DIMENSION M(10, 1)

C Pivot is the last value in the array
      P = M(L, 1)

C Partition index is returned by running PART (not a function)

C Make I one less that the starting value
      I = F - 1

C Loop through every value in the array
      DO 1 J = F, L

C If the value at J is smaller than the pivot, swap with index I
      IF(M(J, 1) .GT. P) GOTO 1


C Increment I to keep track of indexes
      I = I + 1

C Swap the values of indexes I and J to move smaller values below pivot
      T = M(I, 1)
      M(I, 1) = M(J, 1)
      M(J, 1) = T

1     CONTINUE


C Do final swap to put pivot in its proper index
      T = M((I + 1), 1)
      M((I + 1), 1) = M(L, 1)
      M(L, 1) = T

C Return index of pivot
      P = I + 1
      
      QS = P
      RETURN
      END
C ---------------------------------------------------------------------

