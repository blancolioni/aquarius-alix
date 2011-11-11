int gcd(int x, int y)
{
  int t;
  while (y != 0)
  {
     t = y;
     y = x % y;
     x = t;
  }
  return x;
}
