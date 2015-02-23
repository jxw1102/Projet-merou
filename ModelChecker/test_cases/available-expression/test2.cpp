/**
 * Disponibilité de a + b : { 9 }
 */
#include <iostream>
int main(int argv, char** argc) {
    int a = 5;
    int b = 10;
    int x = a + b;
    while (a + b < 5) {
        a++;
        x = a + b;
    }
}