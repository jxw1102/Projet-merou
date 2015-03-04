
int f() { return 2; }

int main(int argc, char **argv) {
	int secret, guess;
	secret = 10, guess = 8;

	int *list = new int[10+f()];
	char *c = new char;
	new double;
	if (guess < secret) {
		delete[] list;
		return 1;
	}
	delete[] list;

	return 0;
}
