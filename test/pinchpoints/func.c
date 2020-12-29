int foo () {
  return 42;
}

int main () {
  int x = 3;
  x = 32;
  x = foo();
  return x;
}
