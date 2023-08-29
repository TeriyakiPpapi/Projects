#include <stdio.h>
#include <stdlib.h>
#define STUDENTS 3
#define EXAMS 4

//function prototypes
void minimum( int grades [][EXAMS], int pupils, int tests);
void maximum( int grades [][EXAMS], int pupils, int tests);
void average( int grades [][EXAMS], int pupils, int tests);
void printArray( int grades [][EXAMS], int pupils, int tests);

int main()
{
    //the array processGrades stores the four pointers to each of the functions
    void (*processGrades[ 4 ])( int [][EXAMS] ,  int, int) = { printArray, minimum, maximum, average};

    int choice;
    int studentGrades[ STUDENTS ][ EXAMS ] = { { 77, 68, 86,73}, { 96, 87, 89, 78 }, { 70, 90, 86, 81}};

    printf( "\nEnter a choice:\n\t0 Print the array of grades\n\t1 Find the minimum grade\n\t2 Find the maximum grade\n\t3 Print the average on all tests for each students\n\t4 End program: ");
    scanf( "%d", &choice );

    while ( choice >= 0 && choice < 4 )
    {
        (*processGrades[choice]) (studentGrades, STUDENTS, EXAMS);
        printf( "\nEnter a choice:\n\t0 Print the array of grades\n\t1 Find the minimum grade\n\t2 Find the maximum grade\n\t3 Print the average on all tests for each student\n\t4 End program: ");
        scanf( "%d", &choice );
    }
    return 0;
}
void minimum( int grades[][EXAMS], int pupils, int tests)
{
    int i; //counter student
    int j; //exam counter
    int lowGrade = 100; // initial.. to highest possible grade
    //loop through rows of grades
    for ( i = 0; i < pupils; i++ )
    {
        //loop for column grades
        for (j = 0; j < tests; j++)
        {
            if (grades[ i] [ j] < lowGrade ){
                lowGrade = grades[i][j];
            }
        }
    }
    printf( "\nLowest grade: %d\n", lowGrade);
}

    //for maximum grade
    void maximum( int grades[] [EXAMS ], int pupils, int tests)
    {
        int i;
        int j;
        int highGrade = 0;

        //loop for rows
        for ( i = 0; i < pupils; i++ )
        {

            //loop for columms
            for (j = 0; j < tests; j++ )
            {

                if ( grades[i][j] > highGrade)
                {
                    highGrade = grades[i ][j ];
                }
            }
        }
        printf( "\nHighest grade: %d\n", highGrade);
    }
    //determine the average grade for a particular student

    void average( int grades [][EXAMS], int pupils, int tests)
    {
        int i,j;
        int total;
        for( i = 0; i < pupils; i++)
        {
            total=0;
            for(j=0;j<tests;j++)
            {
                total += grades[i][j];
            }
            printf( "\tThe average for student %d is %.2f\n", i, ( double ) total / tests );
            }
        }
        void printArray( int grades [][EXAMS], int pupils, int tests)
        {
            int i;
            int j;
            printf( "\n");
            //output for column heads
            printf( "           [0] [1] [2] [3]");

            //output for grades in table format
            for ( i = 0; i < pupils; i++ )
            {

                printf( "\nstudentGrades[%d] " , i );
                for ( j = 0;j < tests; j++ )
                {
                    printf( "%-5d", grades[ i] [ j ]);
                }
            }
        }

