all: z1_test z2_test z4_test z5_test z6_test

# C
z1_test: modules/z1.c z1_test.c
	gcc -Wall z1_test.c modules/z1.c -o z1_test.out

# Ada
z2_test: modules/z2.adb
	gnatmake -Imodules/ -o z2_test.out z2_test.adb

# z3 - Python
# python3 z3_test.py

# Ada to C
z4_test: modules/z4.c z1_test.c modules/z2.adb
	gcc -c z4_test.c modules/z4.c 
	gnatmake -c modules/z2.adb
	gnatbind -n z2.ali
	gnatlink z2.ali z4_test.o z4.o -o z4_test.out

# C to Ada
z5_test: modules/z1.c z5_test.adb
	gcc -c modules/z1.c
	gnatmake -Imodules/ z5_test.adb -o z5_test.out -largs z1.o

# C to Python
z6_test: modules/z1.c
	gcc -fPIC -shared -o z6.so modules/z1.c
# python3 z6_test.py

z7_test: modules/z7.c z7_test.c modules/z3.py
	gcc -o z7_test.out z7_test.c modules/z7.c -I/usr/include/python3.10 -lpython3.10

clean:
	@rm -rf *.out *.ali *.o b~* *.so