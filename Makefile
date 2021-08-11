build:
	@ cargo build

genobj:
	@ cp ./target/debug/bare .
	@ ./bare

testbin: build genobj
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so
	@ gcc output.o libbare.so -Xlinker -rpath ./ -o main
	@ ./main

testclib: build genobj
	@ g++ examples/main.cpp lib/libbare.cpp output.o -Xlinker -rpath ./ -o main
	@ ./main

testrlib: build genobj
	@ gcc -c examples/main.cpp -o main.o
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so
	@ gcc main.o output.o libbare.so -Xlinker -rpath ./ -o main
	@ ./main

dump:
	@ objdump -xsd ./output.o

.PHONY: clean
clean:
	@ rm -f *.so *.o bare main
