int main()
{
    int j = 0;
    int i = 2;
    switch (i) {
        case 1: j++;i++;
                break;
        case 2: j+=2;
                break;
        case 3: j+=3;
                break;
        case 4:
        case 5: j+=10;
                break;
        case 6: j-=1;
        case 7: j++;
        case 8: j+=2;
        case 9: j+=3;
        case 10:
        case 11: j+=10;
            break;
        case 12: j-=1;
        case 13: break;
    }
}