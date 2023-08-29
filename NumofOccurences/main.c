#include <stdio.h>
#include <string.h>
#include <stdlib.h>


char *removeFirst(char *str)
{
char *strDes = malloc(strlen(str) - 1);
for (int i = 1; i < strlen(str); i++)
{
strDes[i - 1] = str[i];
}
return strDes;
}
int main()
{
    //the text that the program will use to determine the number occurences
char *str = "That quality combined with abundant real-world learning opportunities mean you start building your resume almost as soon as you set foot on campus and you’re career-ready when you leave campus. SEMO doesn’t want to just be affordable or just be high quality. We want to be a smart investment, so you launch an extraordinary career and can excel on day one. ";
// The letter to be searched
const char ch = 'b';
int count = 0;
char *result;
while (strlen(str) > 0)
{
str = strchr(str, ch);
if (str == NULL)
break;
char *strDes = malloc(strlen(str) - 1);
str = removeFirst(str);
count++;
}
//display the number of occurence in the texts
printf("Char |%c| founds %d times ", ch, count);
return (0);
}
