all: z1

z1: lexer.l parser.y
	flex -o z1.c lexer.l
	bison -d parser.y
	gcc -o z1 z1.c parser.tab.c -lfl

clean:
	@rm *.c *.h z1
