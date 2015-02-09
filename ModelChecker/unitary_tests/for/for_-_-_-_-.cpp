int f() {
	return 3;
}

int main(int argc, char** argv) {
	// if I replace it with f() it creates only one f() node
	int j = 5 + f();
	for (;;);
	f();
}
