//
// Created by jeuxj on 13/11/2022.
//

#include "Core.h"
#include <iostream>
#if defined(WIN32) && WIN32
#include <Windows.h>
#endif

void PadusetCore_Hi() {
#if defined(WIN32) && WIN32
    SetConsoleOutputCP(CP_UTF8);
#endif

    const char* pauseStart = std::getenv("PADUSET_PAUSE_START");
    if (pauseStart != nullptr && pauseStart[0] != '0') {
        std::cout << "Paduset program started! Press any key.\n";
        std::cin.get();
    }
}