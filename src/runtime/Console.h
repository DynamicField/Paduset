#ifndef PADUSET_CONSOLE_H
#define PADUSET_CONSOLE_H

#include <stdint.h> // NOLINT(modernize-deprecated-headers)
#include "PadusetString.h"

#ifdef __cplusplus
extern "C" {
#endif

int PadusetConsole_ReadInt();
float PadusetConsole_ReadReal();
void PadusetConsole_ReadString(PadusetString* out);
void PadusetConsole_Printf(const char* str, ...);

#ifdef __cplusplus
}
#endif

#endif //PADUSET_CONSOLE_H
