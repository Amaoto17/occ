int printf();

int main() {
    int x = 3;
    int *p = &x;
    printf("%d\n", *p);
    *p = 4;
    printf("%d\n", *p);
    int **q = &p;
    **q = 5;
    printf("%d\n", **q);
    return 0;
}
