int gcd(int x, int y, int z, int w, int q)
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
