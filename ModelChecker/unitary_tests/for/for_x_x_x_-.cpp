int f() {
    return 3;
}

int main(int argc, char** argv) {
    int j = 5 + f();
    for (int i=0 ; i<5 ; i++);
    f();
}
