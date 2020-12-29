int main () {
  int x = 3;
  foo: x = 22;
  return;
  x = 42;
  goto foo;
}
