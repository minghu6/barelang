// #include <iostream>

extern "C" {
    signed long int sum(signed long int, signed long int, signed long int);
}

// extern "C" __int64_t printi(__int64_t x) {
//     printf("++%ld++", x);
//     return x;
// }

int main() {
    //std::cout << "sum 1 2 3 " << sum(1, 2, 3) << std::endl;
    sum(1, 2, 3);
}
