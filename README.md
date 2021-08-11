## Compile

### output.o is lib

`g++ examples/main.cpp lib/libbare.cpp output.o -Xlinker -rpath ./ -o main`

`rustc --crate-type cdylib lib/libbare.rs -o libbare.so`

`gcc -fPIC -shared libbare.so output.o  -o liboutput1.so`

#### lib is cpp

`gcc -fPIC -shared lib/libbare.cpp -o libcbare.so`

`gcc -fPIC -shared lib/libbare.cpp output.o -o liboutput1.so`

`gcc -fPIC -shared libcbare.so output.o  -o liboutput1.so`

`g++ examples/main.cpp liboutput1.so -Xlinker -rpath ./ -o main`

### output.o is bin

`echo $?` print exit code of last exec