#include <unistd.h>

int main(){
    setuid(0);
    char* args[] = {"bash", NULL};
    execvp("/bin/bash", args);
    return 0;
}

// chown root:root a.out
// chmod u+s+x a.out
