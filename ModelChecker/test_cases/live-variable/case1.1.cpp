#include <iostream>

int main(int argc, char** argv) {
    int x = 50;
    int y = 10;
    if (y > 5)
        x = 9;
    else
        x = 8;
    std::cout << x << std::endl;
    return 0;
}

// x : {4}
// y : null