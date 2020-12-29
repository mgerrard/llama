int main () {
  int x = 3;
  foo: x = 22;
  x = 42;
  if (x) {
    goto foo;
  } else {
    return 3;
  }
}
