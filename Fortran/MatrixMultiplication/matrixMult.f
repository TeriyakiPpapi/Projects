C This program is to get input from the user to create matrices
C Then the program will mulitply the matrices

C I apologize ahead of time for the line number confusion:
C I started printing out the prompts for the user to fill out and
C was going sequentially before I remembered that the subroutine has to
C be made before it can be called so that is why it goes from 6 to 90
C and then 400 to 7 and then 12 to 450 and then 800 to 13
C If this is too bothersome, feel free to change it, but I'm just being lazy 
      
C This subroutine is being initialized for later use to use with Matrix A
      SUBROUTINE makeDoubleArrayA(rows, columns)
      DIMENSION M(rows, columns)
      REAL rows
      REAL columns
C The code below is the loop to get the user's numbers
      DO 200 I=1,2
      DO 100 J=1,2
50    FORMAT("Enter a value: ")
      READ(*,50) values
      M(I,J) = values
100   CONTINUE
200   CONTINUE   
C The code below is printing out matrix A    
300   FORMAT(F1.1,F1.1)
      DO 400 I=1,2
      WRITE(*,300) M(I,1), M(I,2)
400   CONTINUE
      END
      
C This subroutine is being initialized for later use to use with Matrix B
      SUBROUTINE makeDoubleArrayB(rows, columns)
      DIMENSION N(rows, columns)
      REAL rows
      REAL columns
C The code below is the loop to get the user's numbers
      DO 600 I=1,2
      DO 500 J=1,3
450   FORMAT("Enter a value: ")
      READ(*,450) values
      N(I,J) = values
500   CONTINUE
600   CONTINUE
C The code below is printing out matrix B
700   FORMAT(F1.1,F1.1,F1.1)
      DO 800 I=1,2
      WRITE(*,700) N(I,1), N(I,2), N(I,3)
800   CONTINUE
      END

C This subroutine is being made to mutiply matrices       
      SUBROUTINE multiplyMatrices(rows, columns, matA, matB)
      DIMENSION matA(2,2)
      DIMENSION matB(2,3)
      DIMENSION P(rows, columns)
      REAL rows
      REAL columns
C The code below is loop to multiply matrix elements together
      DO 1100 I=1,2
      DO 1000 J=1,3
      DO 900 K=1,3
      P(I,J) = matA(K,J) * matB(I,K)
900   CONTINUE
1000  CONTINUE
1100  CONTINUE
C The code below is printing out the mulitplied matrix
      WRITE(*,1200)
1200  FORMAT("A x B =")  
1300  FORMAT(F1.1, F1.1, F1.1)
      DO 1400 I=1,2
      WRITE(*,1300) P(I,1), P(I,2), P(I,3)
1400  CONTINUE
      END

C First we have to get input from the user to create matrix A
      WRITE(*,1)
      WRITE(*,2)
      WRITE(*,3)
1     FORMAT("Matrix A")
2     FORMAT("--------")
3     FORMAT("Rows: ")
      READ(*,4) Arows
4     FORMAT(F1.1)
      WRITE(*,5)
5     FORMAT("Columns: ")
      READ(*,6) Acolumns
6     FORMAT(F1.1)

C This will create matrix A with values user wants      
      CALL makeDoubleArrayA(Arows, Acolumns)

C Next we have to get input from the user to create matrix B 
      WRITE(*,7)
      WRITE(*,8)
      WRITE(*,9)
7     FORMAT("Matrix B")
8     FORMAT("--------") 
9     FORMAT("Rows: ")
      READ(*,10) Brows
10    FORMAT(F1.1)
      WRITE(*,11)
11    FORMAT("Columns: ")
      READ(*,12) Bcolumns
12    FORMAT(F1.1)

C This will create matrix B with values user wants
      CALL makeDoubleArrayB(Brows, Bcolumns)
    
C The code below will determine if Acolumns and Brows are same      
      IF(Acolumns.EQ.Brows) GOTO 14
      WRITE(*,13) 
13    FORMAT("Incompatible Dimensions")
      RETURN
C This will create the multiplied matrix      
14    CALL multiplyMatrices(Arows, Bcolumns, M, N)
      RETURN
      END
