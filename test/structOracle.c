struct fruit{
    char name[20];
    int weight;
};

typedef struct orchard{
    struct fruit fruits[10];
} orchard;

typedef struct {
    orchard orchards[10];
} orchards;

int main(){
    struct fruit apple1 = {"red delicious", 2};
    struct fruit apple2 = {"granny smith", 2};

    orchard myOrchard = {apple1, apple2};

    orchards myOrchards = {myOrchard};

    return 0;
}