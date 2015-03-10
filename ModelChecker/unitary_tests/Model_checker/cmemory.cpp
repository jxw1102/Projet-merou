#include <stdlib.h>

int main() {
	double *pd = (double *) malloc(80 * sizeof(double));
	float *pf = (float *) malloc(80 * sizeof(float));
	malloc(80 * sizeof(char));
	free(malloc(80 * sizeof(int)));

	int a = 10, b = 2;
	if (a > b) {
		free(pd);
		return 1;
	}

	free(pd);
	free(pf);
	return 0;
}
