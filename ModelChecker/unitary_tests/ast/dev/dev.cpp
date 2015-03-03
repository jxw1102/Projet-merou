#include <iostream>

int f(int a, int b){
	return 10;
}

void g() { }

void h(int a) {
	a = 0;
}

float fun() {
	return 1.0f;
}

struct point {
	int x;
	int y;
	point() {}
	point(int a, int b) : x(a), y(b) {}
	~point() {}
};

typedef struct point pt_alias;

enum color {
	RED=9,
	GREEN,
	BLUE
};

typedef enum color color_alias;

typedef enum _fruit {
	APPLE,
	PEAR,
	BANANA
} fruit;

typedef struct _coordinate {
	double lat;
	double lng;
	struct point p;
	fruit f;
} coordinate;

union number {
    int entier;
    double reel;
};

int main(int argc, char* argv[]) {
//	g();
//	int i = f(5,6) + 3;
//	f(3,4);
//	int n = sizeof(i*f(1,2));
//	double *ptr = new double;
//	if (fun()) i = 0;
//	else       fun();
//	while (f(5,fun())) {
//		f(1,6) + 1;
//		if (f(4,2) + fun()) 5*(i - 9*fun());
//	}
//	delete ptr;
//	struct point *pt2 = new pt_alias[10];
//	h((pt2+2)->y);
//	pt_alias pt3 = {4,5};
//	struct point pt1(1,2);
//
//	int *list = new int[10+1];
//	delete[] list;
//	coordinate c = {2.0, 1, pt_alias(2,3), APPLE};
	int i = 0;
	std::string s = "wtf!";
	std::cout << s << std::endl;

	std::cin >> i;

	return 0;
}
