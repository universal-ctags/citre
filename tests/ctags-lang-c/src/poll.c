struct client_stats {
  unsigned int poll;
  unsigned int run;
};

struct polling_target {
  int (* poll) (void *);
};

int poll (struct polling_target *target, void *data)
{
  return 0;
}

int run;
#define run() 1
