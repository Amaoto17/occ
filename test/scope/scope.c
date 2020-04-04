int printf();

int main() {
    int x = 3;
    printf("%d\n", x);
    {
        int x = 5;
        printf("%d\n", x);
    }
    printf("%d\n", x);
    return 0;
}
