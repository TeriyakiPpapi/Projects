#include <stdio.h>

double gap(int, int, int);

void min(int, int, int);
void max(int, int, int);
void average(int, int, int);


int main()
{
  int a, b, c;
  a=10;
  b=15;
  c=30;
  //define a pointer to a function.

  double (*g)(int, int, int) = gap;                   //    double (*g)(int, int, int) = &gap;

  printf("\nzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");
  double k;
  k=gap(a,b,c);
  printf("\ngap =(%d, %d, %d)=%.2lf",a, b, c,k);

  double h=(*g)(a, b, c);
  printf("\n(*g)=(%d, %d, %d)=%.2lf",a, b, c,h);



  printf("\n##############################");

  //define  an array of pointers to functions.

  void (*f[3])(int, int, int)={min, max, average}; //define an array of pointers to functions. void (*f[3])(int, int, int)={&min, &max, &average};




  min(a,b,c);
  (*f[0])(a,b,c);



  printf("\n===============================");
  max(a,b,c);
  (*f[1])(a,b,c);

  printf("\n*******************************");
  average(a,b,c);
  (*f[2])(a,b,c);



  return 0;
}

double gap(int a, int b, int c){
    int x, y;

    // x = min(a, b, c)
    x= a;
	if(x>b){
		x=b;
	}

	if(x>c)
		x=c;

	// y= max(a,b,c)
    y= a;
	if(y<b){
		y=b;
	}

	if(y<c)
		y=c;

	return (double) (y-x);

}

void min(int a, int b, int c){
	int x;
	x= a;
	if(x>b){
		x=b;
	}

	if(x>c)
		x=c;

	printf("\nMin(%d,%d,%d)=%d",a,b,c,x);
}

void max(int a, int b, int c){
	int x;
	x= a;
	if(x<b){
		x=b;
	}

	if(x<c)
		x=c;

	printf("\nMax(%d,%d,%d)=%d",a,b,c,x);
}

void average(int a, int b, int c){
	double x=0.0;
	x= (double)(a+b+c)/3;  //Type Conversion


	printf("\nMax(%d,%d,%d)=%.2lf",a,b,c,x);  //"%.2lf " formate outputs.
}

