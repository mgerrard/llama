int main () {
  int x = 0;
  
  for (int j = 0; j < pat_len; j++) {
    pat[j] = __VERIFIER_nondet_int();
  }
  
  if (foo) {
    bar();
  } else {
    baz();
  }
  
  return 42;
}
