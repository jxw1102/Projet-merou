/**
 * Disponibilité de a + b : { 10, 11 }
 */
#include <iostream>
int main(int argv, char** argc) {
    int a = 5;
    int b = 10;
    int x = a + b;
    int y = (a + b) + x;
    a += 1;
    y = (a + b) + x;
}