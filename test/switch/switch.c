int printf();

int f(int x) {
    switch (x) {
        case 0:
            printf("zero\n");
            break;
        case 1:
            printf("one\n");
        case 2:
            printf("two\n");
            break;
        default:
            printf("something\n");
            break;
    }
    return 0;
}

int main() {
    f(0);
    f(1);
    f(2);
    f(3);
    return 0;
}
