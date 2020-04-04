int printf();

int main() {
    int arr[3];
    arr[0] = 3;
    printf("%d\n", arr[0]);
    *(arr + 1) = 8;
    printf("%d\n", arr[1]);
    arr[1 + 1] = 2;
    printf("%d\n", 2);
    return 0;
}
