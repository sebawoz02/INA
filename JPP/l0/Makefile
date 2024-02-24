all: c ada java go haskell lisp sml prolog

c: hello.c
	@gcc hello.c -o c.out

ada: hello.adb
	@gnatmake -o ada.out hello.adb

java:
	@javac Hello.java

go:
	@go build -o go.out hello.go

haskell:
	@ghc -o haskel.out hello.hs

lisp:
	@echo "To run Common Lisp program type 'clisp' and '(load \"hello.lisp\")'."

sml:
	@echo "To run Standard ML program type 'sml' and 'use \"hello.sml\";'. "

prolog:
	@echo "To run Prolog program type 'swipl' and '[hello]'."

clean:
	@rm -rf *.out *.o *.ali *.class *.hi