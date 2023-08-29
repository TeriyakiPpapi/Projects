#include <stdio.h>
#include <stdlib.h>

int main()
{
    enum Company { GOOGLE, FACEBOOK, XEROX, YAHOO, EBAY, MICROSOFT };

    enum Company xerox = XEROX;
    enum Company google = GOOGLE;
    enum Company ebay = EBAY;

    printf("The value of the xerox is: %d\n", xerox);
    printf("The value of the google is: %d\n", google);
    printf("The value of ebay is: %d\n", ebay);

    return 0;
}
