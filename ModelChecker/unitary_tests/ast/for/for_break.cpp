void f() { }

int main(int argc, char** argv) {
    f();
    for (int i=3 ; 5 < i ; i++)
        break;
    f();
}
