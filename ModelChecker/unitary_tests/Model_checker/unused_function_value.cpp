int f(int a, int b){
	return 1;
}

void g() { }

void h(int a) {
	a = 0;
}

float fun() {
	return 1.0f;
}

int main(int argc, char** argv) {
	g();
	int i = f(5,6) + 3;
	f(3,4);
	if (fun()) i = 0;
	else       fun();
	while (f(5,fun())) {
		f(1,6) + 1;
		if (f(4,2) + fun()) 5*(i - 9*fun());
	}
	return 0;
}
