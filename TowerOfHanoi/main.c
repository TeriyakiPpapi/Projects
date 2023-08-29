#include<stdio.h>

int TowersOfHanoi(int size,int x,int y,int z)
{
   if(size>0)
   {
       TowersOfHanoi((size-1),x,z,y);
           printf("%d -> %d\n",x,z);
       TowersOfHanoi((size-1),y,x,z);
   }
}
int main()
{
   int size;
   int x=1,y=2,z=3;
   printf("Enter Size of the Tower: ");
   scanf("%d",&size);
   TowersOfHanoi(size,x,y,z);
   return 0;
}
