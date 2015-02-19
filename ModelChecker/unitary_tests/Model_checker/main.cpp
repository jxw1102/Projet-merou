int f() {
    return 3;
}

int main(int argc, char** argv) {
    int j = 5 + f();
    f();
    j = 0;
}
