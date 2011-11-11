#include <stdio.h>

int gcd(int,int);

int
main(int argc, char**argv)
{
  int x = 4235;
  int y = 125;
  int z = gcd(x,y);
  printf("%d\n", z);
}
