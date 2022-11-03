//
// Created by jeuxj on 12/11/2022.
//

#include "Console.h"
#include <iostream>
#include <string>
#include <cstdarg>

int PadusetConsole_ReadInt() {
    int result = 0;
    std::cin >> result;
    return result;
}

float PadusetConsole_ReadReal() {
    float result = 0;
    std::cin >> result;
    return result;
}

void PadusetConsole_ReadString(PadusetString* out) {
    std::string str;
    std::getline(std::cin, str);
    PadusetString_Make(str.c_str(), out); // Not very optimized
}

void PadusetConsole_Printf(const char* str, ...) {
    va_list args;
    va_start(args, str);
    vprintf(str, args);
    va_end(args);
}
