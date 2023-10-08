#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <fcntl.h>

#define LINE_LEN 1024

void sig_handler(int sigid) {
	if(sigid==2){}
}

void redirect(char* argz[],int len, int in_out_err){
	char* command[len+1];
	char* file_name;
	int j;
	for(j=0;j<len+1;j++){
		if(strcmp(argz[j], "<")==0 || strcmp(argz[j], ">")==0 || strcmp(argz[j], "2>")==0){
			file_name = argz[j+1];
			break;
		}
		command[j] = argz[j];
	}
	int file;
	if(in_out_err == 0){
		file = open(file_name, O_RDONLY);
	}
	else{
		file = open(file_name, O_WRONLY | O_CREAT, 0777);
	}
	if(file == -1){
		fprintf(stderr, "%s :Could not open file\n", file_name);
		exit(0);
	}
	int file2 = dup2(file, in_out_err);
	close(file);
	if(execvp(command[0], command)== -1){
		fprintf(stderr, "%s: command not found\n", command[0]);
	}
}

int main(int argc, char *argv[]) {
    char wiersz[LINE_LEN];
    char *args[LINE_LEN / 2 + 1];
    int background;
    signal(2, sig_handler);
    while (1) {
    	int potok = 0;
    	int przek_wyjscia = 0;
    	int przek_wejscia = 0;
    	int przek_bledu = 0;
        char cwd[1024];
        getcwd(cwd, sizeof(cwd));
        printf("\033[0;31m[Mylsh]:\033[0;35m%s>\033[0;37m ", cwd);
        if (fgets(wiersz, LINE_LEN, stdin) == NULL) {
            // jeśli użytkownik nacisnął Ctrl + D, to wyjdź z programu
            fprintf(stderr, "Lsh exit!\n");
            exit(0);
	}    
        int i = 0;
        args[i] = strtok(wiersz, " \n\t");
        while (args[i] != NULL) {
            i++;
            args[i] = strtok(NULL, " \n\t");
            if(strcmp(args[i-1], "|")==0){potok=1;}
            if(strcmp(args[i-1], ">")==0){przek_wyjscia=1;}
 	    if(strcmp(args[i-1], "<")==0){przek_wejscia=1;}
 	    if(strcmp(args[i-1], "2>")==0){przek_bledu=1;}
        }
        if (args[0] == NULL) {
            continue;
        }
        // sprawdzenie czy proces ma być wykonywany w tle
        background = 0;
        if (strcmp(args[i - 1], "&") == 0) {
            args[i - 1] = NULL;
            background = 1;
        }
        // jeśli użytkownik wpisał komendę exit
        if (strcmp(args[0], "exit") == 0) {
            fprintf(stderr, "Lsh exit!\n");
            exit(0);
        }
        // jeśli użytkownik wpisał komendę cd
        if (strcmp(args[0], "cd") == 0) {
            if (args[1] == NULL) {
                char *home_dir = getenv("HOME");
                chdir(home_dir);
            } else {
                chdir(args[1]);
            }
            continue;
        }    
       //jezeli nalezy stworzyc potok
       	if(potok==1){
       		printf("PIPES ARE NOT IMPLEMENTED\n");
       	}
	else{	
		int status;
		pid_t child_pid = fork();
		if (child_pid == 0) {
			if(przek_wyjscia==1){
			    redirect(args, i, 1);
			    exit(0);
			}
			if(przek_wejscia==1){
			    redirect(args, i, 0);
			    exit(0);
			}
			if(przek_bledu==1){
			    redirect(args, i, 2);
			    exit(0);
			}
		    	if (execvp(args[0],args) == -1) {
		       	    fprintf(stderr, "%s: command not found\n", args[0]);
		    	}
		    	exit(0);	    
		} else {
			//rodzic wyswietla pid procesu dziecka jezeli proces wykonuje sie w tle, w przeciwnym wypadku czeka na zakonczenie dzialania przez dziecko
		    if (background == 0) {
		        waitpid(child_pid, &status, 0);
		    } else {
		    	signal(SIGCHLD,SIG_IGN);
		        printf("[%d]\n", child_pid);
		    }
		}
        }
    }
    return 0;
}
