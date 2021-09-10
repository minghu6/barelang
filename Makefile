BARE_COMPILER=bac

build:
	@ cargo build

copybin:
	@ cp ./target/debug/${BARE_COMPILER} .

getbac: build copybin

genlib:
	@ cargo build -p rsc --release
	@ cp ./target/release/librsc.so .

testexp0: build copybin
	@./${BARE_COMPILER} ./examples/exp0.ba
	@ ./a.out

testforeignapi:
	cargo test -- --nocapture test_rsclib

dump:
	@ objdump -xsd ./output.o

.PHONY: clean
clean:
	@ rm -f *.so *.o ${BARE_COMPILER} main *.out *.d *.ll
