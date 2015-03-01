int f(int a, int b){
	return 1;
}

int main(int argc, char** argv) {
	int *p, *q;
	p = p + 1;

	int *r = p + 1;

	const int* s;
	s = 0 ? 0 : s + 1;

	float f = -3;

	f += 5;

	double arr[] = { 3,2,1,4};
	double a = arr[0];
	float *z;
	float *y = z++;
	float *x;
	x += 3;
	x = x + 6;
	return 0;
}
