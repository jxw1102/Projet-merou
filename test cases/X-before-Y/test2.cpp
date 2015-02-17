/**
 * Reconnaissance du motif f(x) ... g(y)
 * Résultat attendu : true
 */
#include <iostream>
void f(int x) {}
void g(int x) {}
int main(int argv, char** argc) {
    int a = 10;
    if (a > 5)
        g(13);
    else 
        g(17);
}