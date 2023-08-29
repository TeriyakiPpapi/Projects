#include <stdio.h>
#include <stdlib.h>
#include <time.h>
int main()
{
    int num = 0;
    int guess = 0;
    int tries = 5;
    srand(time(0));
    num = rand() % 20 + 1;
    printf("\nYou have %d tries left.");
    printf("Guess the number: \n");

    do
    {
        printf("Enter a guess between 1 and 20:");
        scanf("%d", &guess);
        tries++;

        if (guess > num)
        {
            printf("Too high!!");
        }
        else if(guess < num)
        {
            printf("Too low!");
        }
        else
        {
            printf("\nCorrect!!! you got it in %d guesses!\n", tries);
        }

    }while (guess != num);
    return 0;
}
