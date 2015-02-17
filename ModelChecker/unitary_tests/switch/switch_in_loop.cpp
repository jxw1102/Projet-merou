int main()
{
    int j = 0;
    while (j > 5) {
        int i = 2;
        switch (i) {
            case 1: j++;
            case 2: j+=2;
            case 3: j+=3; i++; break;
            case 4:
            case 5: j+=10;
                    break;
            case 6: j-=1;
        }
    }
    int k = 7;
}
