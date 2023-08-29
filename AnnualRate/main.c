#include <stdio.h>
#include <stdlib.h>

int main()
{
    /*declare the variables*/
    float principal, interestRate, interest;
    int days;

    printf( "Enter loan principal (-1 to end): " );
    /*allow user scan*/
    scanf( "%f", &principal );

    while(principal != -1)
    {

        printf( "\nEnter interest rate: " );
        scanf( "%f", &interestRate );
        printf( "\nEnter term of the loan in days: " );
        scanf( "%d", &days );
        interest = ( principal * interestRate * days ) / 365;
        printf( "\nThe interest charge is $%.2f", interest);
        printf( "\nEnter loan principal (-1 to end): " );
        scanf( "%f", &principal );
    }

    return 0;
}
