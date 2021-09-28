struct rectangle {
  struct point a;
  /* @ */ struct point b;
};

scope get_score(struct game *game)
{
  return game->point;		/* ! */
}
