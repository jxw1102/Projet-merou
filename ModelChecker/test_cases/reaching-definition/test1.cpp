/**
 * Résultat attendu :
 *     - "x = 5"  : { 17,18,19,20,21,23 }
 *    - "y = 17" : { 15,16,18,19 }
 *    - "z = 15" : { 16,18,19,20,22,24,25 }
 *    - "y += 3" : { 19,20 } 
 *    - "x = y"  : { 20 }
 *    - "x = 6"  : { 22 }
 */
#include <iostream>
int main(int argv, char** argc) {
    int x = 5;
    int y = 17;
    int z = 15;
    
    if (x > y) {
        y += 3;
        x  = y;
    } else {
        x  = 6;
    }
    std::cout << "2" << std::endl;
    return 0;
}