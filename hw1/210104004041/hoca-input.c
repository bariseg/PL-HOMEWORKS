int sum(int a, char b, char c);

int bos();

int sum(int a, char b, char c) {
    return sum(a, b, c);
}

functionname(par1, par2, par3, par4);

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
		x = 5;
    }

    for (int i = 0; i < 10; i++) {
        printf("%d\n", i);
    }

    while ( x < 100){
        a = 10;
    }

    return 0;
}
