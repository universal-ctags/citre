struct Guitar *Guitar;
enum codeHint {
  major,
  minor,
  unknown,
};

struct device {
  const char *name;
  unsigned int major;
  unsigned int minor;
};

