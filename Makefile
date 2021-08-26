BARE_COMPILER=bac

build:
	@ cargo build

copybin:
	@ cp ./target/debug/${BARE_COMPILER} .
	# @ ./${BARE_COMPILER}


getbac: build copybin

genlib:
	@ rustc --crate-type cdylib lib/libbare.rs -o libbare.so


testexp0: build copybin
	@./${BARE_COMPILER} ./examples/exp0.ba
	@ ./a.out

dump:
	@ objdump -xsd ./output.o

.PHONY: clean
clean:
	@ rm -f *.so *.o ${BARE_COMPILER} main *.out
