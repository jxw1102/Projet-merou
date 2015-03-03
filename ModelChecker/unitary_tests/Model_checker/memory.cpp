
int f() { return 2; }

int main(int argc, char **argv) {
	int secret, guess;
	secret = 10, guess = 8;

	int *list = new int[10+f()];
	char *c = new char;
	if (guess < secret) {
		return 1;
	}
	delete[] list;

	return 0;
}
