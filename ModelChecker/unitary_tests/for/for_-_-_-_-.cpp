int f() {
	return 3;
}

int main(int argc, char** argv) {
	int j = 5 + f();
	for (;;);
	f();
}
