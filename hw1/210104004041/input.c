int sum(int a, char b, char c);

int bos();

int ihatelisp() {
    return 5 + 14;
}

int sum(int a, char b, char c) {
    return sum(a, b, c);
}

int functionname(int par1,int par2){
    return par1 < par2;
}

int main() {
    int x = 10;
    int y = 20;
    int a = 30;
    int result = sum(x, y, 30);

    result = sum(x, y, 30);
    result = x + y;
    result = x < y;
    result = x > y;
    result = x <= y;
    result = x >= y;
    result = x == y;
    result = x != y;

    if (result > 25) {
        printf("Result is greater than 25\n");
    }

    for (int i = 0; i < 10; i++) {
        printf("%d\n", i);
    }

    while ( x < 100){
        a = 10;
    }

    return 0;
}
