#include <stdio.h>
#include <stdlib.h>
//#include <malloc.h>

    int main()
{
    int main()
{
    int r = 2, c = 4;
    char *Sarray = (char *)malloc(r * c * sizeof(char));
    *(Sarray + 0 * c + 0) = 'A';
    *(Sarray + 0 * c + 1) = 'B';
    *(Sarray + 0 * c + 2) = 'C';
    *(Sarray + 0 * c + 3) = '\0';

    *(Sarray + 1 * c + 0) = 'E';
    *(Sarray + 1 * c + 1) = 'F';
    *(Sarray + 1 * c + 2) = 'G';
    *(Sarray + 1 * c + 3) = '\0';
    int i, j;
    for (i = 0; i < r; i++)
    {
        for (j = 0; j < c; j++)
        {
            printf("%c", *(Sarray + i * c + j));
        }
        printf("\n");
    }
    free(Sarray);
}
