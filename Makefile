BARE_COMPILER=bac

build:
	@ cargo build

copybin:
	@ cp ./target/debug/${BARE_COMPILER} .
	# @ ./${BARE_COMPILER}

testbin: build copybin
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so
	@ gcc output.o libbare.so -Xlinker -rpath ./ -o main
	@ ./main

testclib: build copybin
	@ g++ examples/main.cpp lib/libbare.cpp output.o -Xlinker -rpath ./ -o main
	@ ./main

testrlib: build copybin
	@ gcc -c examples/main.cpp -o main.o
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so
	@ gcc main.o output.o libbare.so -Xlinker -rpath ./ -o main
	@ ./main

testexp0: build copybin
	./${BARE_COMPILER} ./examples/exp0.ba

dump:
	@ objdump -xsd ./output.o

.PHONY: clean
clean:
	@ rm -f *.so *.o ${BARE_COMPILER} main
