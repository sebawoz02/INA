#include <stdio.h>


int main(int argc, char *argv[])
{
	for(int num=0;num<256;num++)
	{	
		printf("\x1b[38;5;%dmHello World!\x1b[0m\n", num);
	}
	return 0;
}
