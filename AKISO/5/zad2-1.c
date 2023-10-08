#include <stdio.h>
#include <signal.h>
#include <unistd.h>

void sig_handler(int sigid) {
	printf("Otrzymano sygnal %d\n", sigid);
}

int main() {
	for (int i = 1; i <= 64; i++) {
		if (signal(i, sig_handler) == SIG_ERR) printf("Nie mozna obsluzyc sygnalu nr %d\n", i);
	}
	return 0;
}
