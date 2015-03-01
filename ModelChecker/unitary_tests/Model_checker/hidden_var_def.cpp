int f(int q) {
    return q+2;
}

int main(int argc, char** argv) {
	int j = 0, q = 2;

	{
		int j = 3;
		{
			float j = 15.0f;
			{
				float j;
				{
					int j = 18;
				}
			}
		}
	}
    if (j == 2)
    	f(q);

    if (false){
    	q = 7;
    }
    else {
    	while (true) {
    		int k = 0;
    	}
    }

    int k;
    return 0;
}
