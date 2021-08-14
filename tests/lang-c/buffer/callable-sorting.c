int
main(void)
{
  struct client_stats s;

  s.poll = 0;			/* @member */

  struct polling_target *t = new_target();
  t->poll(NULL);		/* @callable-member */

  /* @callable-func */
  poll (t, NULL);

  /* @macro */
  run();

  return 0;
}
