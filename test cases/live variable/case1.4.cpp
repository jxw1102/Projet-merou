#include <iostream>

int main(int argc, char** argv) {
    int x = 50;
    int y = 10;
    x = 40;
    y = 9;
    x += y;
    std::cout << x << std::endl;
    return 0;
}

// x : {4}
// y : {5}