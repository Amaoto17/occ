int printf();

int main() {
    if (0) printf("a");
    if (1) printf("b");

    if (0) printf("c");
    else printf("d");
    if (1) printf("e");
    else printf("f");

    printf("\n");
    return 0;
}
