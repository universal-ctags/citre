int main (void)
{
  find();
  if (found())			/* @call */
    goto found;			/* @goto */

  return 1;
 found:
  return 0;
}
