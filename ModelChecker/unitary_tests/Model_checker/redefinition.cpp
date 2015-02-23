int f(int i) {
 if (i >= 0) goto end;
 return 0;
 end:
 return 1;
}

int main(int argc, char** argv) {
	int *p, *q;
	p = p + 1;	
	
	{
		int *p;
	}
	
	for(int i=0; i< 2; i++){
		float p = 1.2f;
	}
	
	const char* s;
	s = 0 ? "sofia" : "sofiati";
	
	float f = 3;
	
	f += 5;
	
	double arr[] = { 3,2,1,4};
	double a = arr[0];
	float *z;
	return 0;
}