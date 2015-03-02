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

// TODO: struct, enum, typedef, union

struct point {
	int x;
	int y;
	float *data;
};

typedef struct point pt_alias;

enum color {
	RED,
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

// END: struct, enum, typedef, union

int main(int argc, char* argv[]) {
	char* s = "fuck you, bitch!";
	g();
	int i = f(5,6) + 3;
	f(3,4);
	int n = sizeof(i*f(1,2));
	int *list = new int[10];
	double *ptr = new double;
	if (fun()) i = 0;
	else       fun();
	while (f(5,fun())) {
		f(1,6) + 1;
		if (f(4,2) + fun()) 5*(i - 9*fun());
	}
	delete[] list;
	delete ptr;
//	pt_alias pt;
//	h(pt.x);
//	struct point *pt2 = new pt_alias;
//	h(pt2->y);
	return 0;
}
