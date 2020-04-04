int printf();

int main() {
    struct point {
        int x;
        int y;
    } p;
    p.x = 1;
    p.y = 2;
    printf("%d\n", p.x);
    printf("%d\n", p.y);
    struct point *q = &p;
    q->x = 5;
    q->y = 6;
    printf("%d\n", q->x);
    printf("%d\n", q->y);
    return 0;
}
