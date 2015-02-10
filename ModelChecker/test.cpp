static char staVar[100];
int globVar = 0;

int increment(int value) {
	return ++value;
}

void increment(int& value, int steps) {
	value += steps;
}

int self(int value) {
	return value;
}

int main(int argc, char *argv[]) {
	int arr[] = { 22, 34, 3, 32, 82, 55, 89, 50, 37, 5, 64, 35, 9, 70 };
	int len = 14;
	int i, j, temp;
	int sum = 0;
	sum = increment(sum);
	for (int i = 0;;) {
		sum += i;
		if (i == 8)
			break;
		if (i < 8)
			continue;
		sum *= i;
		sum -= i;
	}
	if (sum > 100)
		sum = 100;
	else if (sum > 50) {
		sum = 50;
		return 1;
	} else
		sum -= 5, sum = sum + 10;

	switch (sum / 10) {
	case 1:
		sum = 20;
		break;
	case 2:
		sum = 30;
	default:
		sum += 10;
	}
	self(sum);

label: if (sum == 19 ? (sum = 20, sum -= 10) : sum = 30)
		sum = 10000;
	else {
		for (; sum > 0; sum += 2)
			switch (sum) {
			case 1:
			case 2:
				break;
			case 3:
				sum++;
				for (i = 0; i < len - 1; i++)
					for (j = 0; j < len - 1 - i; j++)
						if (arr[j] > arr[j + 1]) {
							temp = arr[j];
							arr[j] = arr[j + 1];
							arr[j + 1] = temp;
						}
			}
		if (1) {
			if(sum == 10)
				self(sum);
		}
	}

	switch (sum) {
	case 1:
		if (1)
			;
		else
			break;
	case 2:
		break;
	case 3:
		goto label;
	case 4:
	default:
		sum++;
		sum--;
	}

	return 0;
}

