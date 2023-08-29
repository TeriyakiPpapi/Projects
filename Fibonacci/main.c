#include<stdio.h>
int fib_iter(int n)
{
    int j;
    int arr[n + 1];
    arr[0] = 0;
    arr[1] = 1;
    for(j=2;j<=n;j++)
    {
        arr[j] = arr[j-1] + arr[j-2];

    }
    return arr[n];
}
int main()
{
    int i, j, n;
    printf("Enter n : ");
    scanf("%d", &n);
    printf("%dth fibonacci number : %d", n, fib_iter(n));
    return 0;

}
