/**
 * Reconnaissance du motif f(x) ... g(y)
 * Résultat attendu : false { 9,12 }
 */
#include <iostream>
void f(int x) {}
void g(int x) {}
int main(int argv, char** argc) {
    int a = 10;
    f(10);
    if (a > 5)
        g(13);
    f(13);
    f(15);
    g(12);
}