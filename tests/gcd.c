void gcd(void)
{
  int a, b;
  int t;

  a = 34;
  b = 12;

  while (b != 0)
    {
      t = b;
      b = a % b;
      a = t;
    }
}
