int gcd(x)
     int x;
{
  int stack[16];
  stack[0] = x;
  stack[15] = x;
  return stack[0] + stack[15];
}
