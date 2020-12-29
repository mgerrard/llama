int foo () {
  int bar = 4;
  {
    if (bar) {
      bar = 2;
    } else {
      bar = 3;
    }
    return bar;
  }
}

int main () {
  int x = 0;
  {
    {
      foo();
    }
    return 42;
  }
}
