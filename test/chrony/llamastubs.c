int ntohl(int foo) { return 42; }

int stderr = 2;

void arc4random_buf(char *buf, unsigned int len){
  for(int i=0; i<len; i++){
    buf[i] = __VERIFIER_nondet_char();
  }
}

void printf(const char *format, ...){}

double fabs(double x){
  if(x>0) return x;
  else return -x;
}
