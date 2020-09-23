int used(int x){
    int y;
    y = x*x;
    int z = 22;
    return z*y;
}
int main(){
    int myVar = 12;
    myVar = used(myVar);
    int otherVar = used(myVar+2);

    return 0;
}