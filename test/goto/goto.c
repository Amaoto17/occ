int printf();

int main() {
    int i = 0;
loop:
    i++;
    if (i < 3) goto loop;
    printf("%d\n", i);
    int x = 7;
    goto end;
    x = 5;
end:
    printf("%d\n", x);
    return 0;
}
