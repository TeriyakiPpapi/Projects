#include <stdio.h>
#include <stdlib.h>
#define MAX 100

//declare functions
int searchLinear ( const int arrayFunc[], int searchKey, int length);

int main(void)
{
    //initialize array
    int element [ MAX ];
    //declare variables
    int count;
    int searchValue;
    int location;
    /*int searchLinear;*/

    for ( count = 0; count < MAX; count++ )
    {

        element[ count ] = 2 * count;
    }
    //ask user to enter search value
    printf( "Enter search integer:\n" );
    scanf( "%d", &searchValue );
    //locate if searchValue is in array elements
    location = searchLinear ( element, searchValue, MAX );
    //display the result
    if ( location != -1 )
    {

        printf ("Value found in element %d\n", location);
    }
    else
    {
        printf ("Value not located\n");
    }
    return 0;
    }
    int searchLinear ( const int arrayFunc[], int searchKey, int length )
    {

        //declare variables
        int n = length;
        --n;
        //if input is not found
        if ( n < 0 )
        {
            return -1;
        }
        //else
        if ( arrayFunc [ n] == searchKey )
        {

            //return key location
            return n;
        }
        //recursive process
        else{
            return searchLinear( arrayFunc, searchKey, n);
        }
        }

