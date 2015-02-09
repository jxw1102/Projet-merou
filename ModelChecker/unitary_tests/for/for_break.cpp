void f() { }

int main(int argc, char** argv) {
	f();
	// incorrect here, the update node should be isolated but it is connected
	for (int i=3 ; 5 < i ; i++)
		break;
	f();
}
