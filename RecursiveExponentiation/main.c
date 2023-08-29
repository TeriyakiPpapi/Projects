#include <stdio.h>

double power(double base, int exponent){
   //If exponent is negative
   if(exponent < 0){
      return 1.0/power(base, -1*exponent);
   }
   //If exponent is zero
   else if(exponent == 0){
      return 1;
   }
   else{
      //recursive call
      return base*power(base,exponent-1);
   }
}

int main(){
   double b;
   int e;
   printf("Enter base: ");
   scanf("%lf",&base);
   printf("Enter exponent: ");
   scanf("%d",&exponent);
   printf("pow(%.0lf,%d) = %f\n",base,exponent,power(base,exponent));
   return 0;
}
