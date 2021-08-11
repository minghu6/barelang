build:
	@ cargo build

testclib: build
	@ cp ./target/debug/bare .
	@ ./bare
	@ g++ examples/main.cpp lib/libbare.cpp output.o -Xlinker -rpath ./ -o main
	@ ./main

testrlib: build
	@ cp ./target/debug/bare .
	@ ./bare
	@ gcc -c examples/main.cpp -o main.o
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so
	@ gcc main.o output.o libbare.so -Xlinker -rpath ./ -o main
	@ ./main

dump:
	@ objdump -xsd ./output.o

.PHONY: clean
clean:
	@ rm -f *.so *.o bare main
