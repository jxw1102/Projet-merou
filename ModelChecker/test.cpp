//#include <iostream>
//using namespace std;

int fun() {
    return 2;
}

void fun(int a) {
    
}

int main(int argc, char** argv)
{
    int a = 1;
//    while (a < 10) {
//        a++;
    for (;;) {
            if (a%2==0)
                a++;
            fun();
        }
//    }
    fun(a);
    return 0;
}
