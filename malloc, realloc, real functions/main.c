/*#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

int main()
{
    int x = 2, y = 4;
    char *Aarray = (char *)malloc(x * y * sizeof(char));
    *(Aarray + 0 * y + 0) = 'A';
    *(Aarray + 0 * y + 1) = 'B';
    *(Aarray + 0 * y + 2) = 'C';
    *(Aarray + 0 * y + 3) = '\0';

    *(Aarray + 1 * y + 0) = 'E';
    *(Aarray + 1 * y + 1) = 'F';
    *(Aarray + 1 * y + 2) = 'G';
    *(Aarray + 1 * y + 3) = '\0';
    int u, i;
    for (u = 0; u < x; u++)
    {
        for (i = 0; i < y; i++)
        {
            printf("%c", *(Aarray + u * x + i));
        }
        printf("\n");
    }
    free(Aarray);
}*/

/******************************************************************************

malloc, realloc, free functions.

*******************************************************************************/
#include <stdio.h>
#include <malloc.h>

int main()
{
    int row = 2, column = 3;
    int *arrPtr = NULL;
    arrPtr = (int *)malloc(row * column * sizeof(int));

    int i, j, value = 10;
    int offset =0;
    for (i = 0; i <  row; i++)
      for (j = 0; j < column; j++){
          offset = i*column + j;
         *(arrPtr + offset ) = value;
         value += 10;
    }

    for (i = 0; i <  row; i++){
     printf("\nrow[%d]= ", i);
      for (j = 0; j < column; j++) {
         offset = i*column + j;
         printf("%d ", *(arrPtr + offset));
      }
    }


    //To increase memory spaces for the 3rd row
    int *yPtr=NULL;

    yPtr = arrPtr + row * column;
    row = row+1;
    arrPtr = (int *) realloc(arrPtr, row*column * sizeof(int));


    value=100;
    for (j = 0; j < column; j++){
         *( yPtr + j) = value;
         value += 100;
    }

    printf("\n***********");
    for (i = 0; i < row; i++){
      printf("\n>>row[%d]= ", i);
      for (j = 0; j < column; j++) {
         offset = i*column + j;
         printf("%d ", *(arrPtr + offset));
      }
    }

    free(arrPtr); //release memory spaces

    return 0;
}


