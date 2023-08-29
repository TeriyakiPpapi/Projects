#include <stdio.h>
#include <stdlib.h>
#define MAX 1000

int main (void)
{

    //initialize arrays
    int arrayPrimes[ MAX ];

    int count1;
    int count2;
    int primeNum = 0;

    //sets all elements of array to 1
    for ( count1 = 0; count1 < MAX; count1++ )
    {

        arrayPrimes[ count1 ] = 1;
    }
    //tests multiple subscript
    for ( count1 = 2; count1 < MAX; count1++ )
    {
        if ( arrayPrimes [count1 ] == 1)
        {
            //loop remainder
            for ( count2 = count1 + 1;  count2 < MAX; count2++)
            {
                //set multiples to 0
                if ( count2 % count1 == 0)
                {

                    arrayPrimes[count2 ] = 0;
                }
            }
        }
    }
    //displays primes numbers between 2 - 197
    for ( count1 = 2; count1 < MAX; count1++ )
    {

        if ( arrayPrimes [ count1 ] == 1)
        {

            printf( "%3d is prime.\n", count1);
            ++primeNum;
        }
    }
    printf( "\n%d prime numbers found.\n", primeNum );
    return 0;

    }
