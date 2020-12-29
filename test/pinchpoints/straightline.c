int main () {
  int x = 3;
  if (x<0) {
    x = 4;
  } else {
    x = 5;
  }
  while (1) {
    x = 6;
  }
  switch(3) {
  case 1:
    x = 1;
  case 2:
    x = 2;
    break;
  default:
    x = 3;
  }
  return 3;
}
