C_COMPILER= "clang"
C_FLAGS= -lm

RM= rm -rf


all: encoder decoder

encoder:
	@${C_COMPILER} ${C_FLAGS} encoder.c -o encoder.out

decoder:
	@${C_COMPILER} ${C_FLAGS} decoder.c -o decoder.out

clean:
	@${RM} *.out
