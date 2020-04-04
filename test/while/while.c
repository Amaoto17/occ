int printf();

int main() {
    int i = 0;
    int sum = 0;
    while (i < 10) {
        sum += i++;
    }
    printf("%d\n", sum);
    return 0;
}
