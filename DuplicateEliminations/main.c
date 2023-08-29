#include <stdio.h>
#include <stdlib.h>
#define SIZE 20

int main(void)
{

    //initialize array
    int numList [ SIZE ] = { 0 };
    //declare the variables
    int loop;
    int testDupe;
    int count =0;
    int flagDupe;
    int input;

    printf( "Enter 20 numbers from 10 until 100:\n");
    //get numbers from user
    for ( loop = 0; loop <= SIZE - 1; loop++ )
    {
        flagDupe = 0;
        scanf("%d", &input);
        //test for the input
        while (input < 10 || input > 100 )
        {
            printf( "\nInvalid Input. Enter again: ");
            scanf ("%d", &input);
        }
        for (testDupe = 0; testDupe < count; testDupe++ )
        {

            //if there is, break from loop
            if (input == numList [ testDupe] )
            {

                flagDupe = 1;
                break;
            }
        }
        if ( !flagDupe )
        {

            numList[ count++ ] = input;
        }
    }
    printf( "\nNon-duplicate values are:\n");
    //display the array for non dupli....
    for ( loop = 0; loop < SIZE && numList[ loop ] != 0; loop++ )
    {
        printf( "%d", numList[ loop ]);
    }
        printf( "\n");
        return 0;
        }


