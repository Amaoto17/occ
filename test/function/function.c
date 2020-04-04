int printf();

int succ(int x) {
    return x + 1;
}

int add(int x, int y) {
    return x + y;
}

int main() {
    printf("%d\n", succ(1));
    printf("%d\n", add(2, 3));
    return 0;
}
