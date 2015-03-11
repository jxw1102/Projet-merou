void printf(char* s, int b) { }

int main() {
    int a = 1, b = 1;
    while (b < 100) {
		b += a;
		a  = b;
    }
    printf("%d",b);
}
