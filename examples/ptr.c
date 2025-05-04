int main(){
    // Pointer Functionality Testing
    int x = 5;
    int *ptr = &x;

    int y = *ptr;

    printf("Value of x: %d\n", x);
    printf("Address of x: 0x%d\n", &x);
    printf("Value of y: %d\n", y);

    return 0;
}