       IDENTIFICATION DIVISION.
       PROGRAM-ID. QS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INTS-TABLE.
               05 INTS PIC 9(2) OCCURS 10 TIMES.
       01 F PIC 9(2).
       01 L PIC 9(2).
       01 I PIC 9(2).

       PROCEDURE DIVISION.
       DISPLAY "Please enter 10 numbers...".
       PERFORM VARYING I FROM 1 BY 1
       UNTIL I > 10
       DISPLAY "Integer: " WITH NO ADVANCING
       ACCEPT INTS(I)
       END-PERFORM.
       DISPLAY " ".
       SET F TO 1.
       SET L TO 10.
       CALL "QUICKSORT" USING INTS-TABLE, F, L.
       DISPLAY "SORTED:".
           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 10
                   DISPLAY INTS(I)
           END-PERFORM.
       STOP RUN.
       END PROGRAM QS.



       IDENTIFICATION DIVISION.
       FUNCTION-ID. FUNCPART.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION FUNCPART.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PIVOT PIC 9(2).
       01 I PIC 9(2).
       01 J PIC 9(2).
       01 TEMP PIC 9(2).
       LINKAGE SECTION.
       01 INTS-TABLE.
               05 INTS PIC 9(2) OCCURS 10 TIMES.
       01 F PIC 9(2).
       01 L PIC 9(2).
       01 PARTINDEX USAGE BINARY-LONG.

       PROCEDURE DIVISION USING INTS-TABLE, F, L
           RETURNING PARTINDEX.
       SET PIVOT TO INTS(L).
       SUBTRACT 1 FROM F GIVING I.
           PERFORM VARYING J FROM F BY 1
                   UNTIL J IS = L
                   IF INTS(J) <= PIVOT
                           ADD 1 TO I
                           SET TEMP TO INTS(I)
                           SET INTS(I) TO INTS(J)
                           SET INTS(J) TO TEMP
                   END-IF
           END-PERFORM.
       ADD 1 TO I.
       SET TEMP TO INTS(I).
       SET INTS(I) TO INTS(L).
       SET INTS(L) TO TEMP.
       SET PARTINDEX TO I.
       END FUNCTION FUNCPART.



       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICKSORT RECURSIVE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION FUNCPART.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PARTINDEX USAGE BINARY-LONG.
       LINKAGE SECTION.
       01 INTS-TABLE.
               05 INTS PIC 9(2) OCCURS 10 TIMES.
       01 F PIC 9(2).
       01 L PIC 9(2).
                           SET TEMP TO INTS(I)
                           SET INTS(I) TO INTS(J)
                           SET INTS(J) TO TEMP
                   END-IF
           END-PERFORM.
       ADD 1 TO I.
       SET TEMP TO INTS(I).
       SET INTS(I) TO INTS(L).
       SET INTS(L) TO TEMP.
       SET PARTINDEX TO I.
       END FUNCTION FUNCPART.



       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICKSORT RECURSIVE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION FUNCPART.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PARTINDEX USAGE BINARY-LONG.
       LINKAGE SECTION.
       01 INTS-TABLE.
               05 INTS PIC 9(2) OCCURS 10 TIMES.
       01 F PIC 9(2).
       01 L PIC 9(2).

       PROCEDURE DIVISION USING INTS-TABLE, F, L.
       IF F < L
           SET PARTINDEX TO FUNCPART(INTS-TABLE, F, L)
           SUBTRACT 1 FROM PARTINDEX
           SET L TO PARTINDEX
           CALL "QUICKSORT" USING  INTS-TABLE, F, L
           ADD 2 TO PARTINDEX
           SET F TO PARTINDEX
           CALL "QUICKSORT" USING  INTS-TABLE, F, L
       END-IF.
       END PROGRAM QUICKSORT.
                                                                             103,29        Bot
