int func() {
  return 0;
}

int ref1() {
  return func();
}

int main() {
  return func();
}
