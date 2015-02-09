int fun() {
label:
    return 2;
}

void fun(int a) {

}

int main(int argc, char** argv)
{
    int a = 10;
    while (a < 20) {
        a++;
        ++a;
        for (int j = 0; j < 20; j++) {
            j++;
            if (a > 5)
                break;
        }
    }
    fun(a);
    return 0;
}
