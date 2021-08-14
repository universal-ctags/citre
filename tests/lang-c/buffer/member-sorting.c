unsigned int get_major_number (struct device *dev)
{
  return dev->major;		/* @arrow */
}

struct device device_template(unsigned int major)
{
  device d = {
    .name  = "unknown",
    .minor = 0,			/* @dotinit */
  };

  d.major = major;		/* @dotassign */
  return d;
}

enum codeHint get_major_hint (void)
{
  return major;			/* @nomember */
}
