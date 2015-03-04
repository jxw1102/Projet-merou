#include <iostream>
#include <cstdlib>
#include <ctime>

int main(int argc, char **argv) {
	int secret, guess;
	srand(time(NULL));
	secret = rand() % 10 + 1;
	std::cin >> guess;

	FILE *file = fopen("myfile.txt", "w");
	FILE *f2 = fopen("myfile2.txt", "w");

	fclose(fopen("myfile3.txt", "w"));

	if (guess < secret) {
		fclose(file);
		return 1;
	}

	if (fopen("test1","r"));

	std::cout << "blabla" << std::endl;

	fclose(f2);
	fclose(file);

	FILE *a = 1 + fopen("test2", "r");
	fclose(a);

	fopen("blabla", "w");

	return 0;
}
