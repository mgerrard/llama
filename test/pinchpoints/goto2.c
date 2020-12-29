int main () {
  int x = 3;
  x = 42;
  if (x) {
    goto foo;
  }
  if (baz) {
  } else {
    return 3;
  }
  foo: x = 22;
}
