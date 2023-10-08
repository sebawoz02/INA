#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>

//wnioski: Syngały nie są kolejkowane. Proces otrzymal tylko jeden ze 100 sygnałow wyslanych od razu po sobie.

int signals_handled = 0;

void sig_handler(int sigid){
    signals_handled++;
    printf("Otrzymano sygnał numer %d\n", signals_handled);
}


int main()
{
    pid_t pid = fork();
    if(pid==0){ //dziecko
        while(1) {
            signal(SIGUSR1, sig_handler);
            if(signals_handled>10){
                exit(0);
            }
        }
    }
    else{
        int status;
        sleep(2);
        for(int i = 0; i<100; i++) kill(pid, SIGUSR1);
        printf("Rodzic wysłał 100 sygnałów w bardzo małych odstępach czasu.\n");
        printf("Rodzic zaczyna wysyłac 10 sygnałow w odstępie 1 sekundy kazdy.\n.");
        for(int i = 0; i<10; i++){
            kill(pid, SIGUSR1);
            sleep(1);
        }
        waitpid(pid, &status, 0);
    }
    return 0;
}

