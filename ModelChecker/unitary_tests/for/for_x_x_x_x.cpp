int f() {
	return 3;
}

int main(int argc, char** argv) {
	int j = f();
	for (int i=3 ; i<5 ; i++)
		i *= 2;
	f();
}
