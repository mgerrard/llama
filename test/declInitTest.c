int used(int x){
    int y;
    y = x*x;
    int z = 22;
    return z*y;
}

int unused(int a){
    int b;
    b = 2;
    int c = b*a;
    return c;
}

int main(){
    int myVar = 12;
    myVar = used(myVar);
    int otherVar = used(myVar+2);

    return 0;
}