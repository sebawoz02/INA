#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int main() {
	setuid(0);
	printf("%d\n", kill(1, 9)); //mozna wsylac syngal SIGKILL do init ale nie bedzie to mialo zadnego efektu
	return 0;
}
