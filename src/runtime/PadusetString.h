#ifndef PADUSET_PADUSETSTRING_H
#define PADUSET_PADUSETSTRING_H

#include <stdint.h> // NOLINT(modernize-deprecated-headers)

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    char* buffer;
    int32_t length;
    int32_t bufferSize;
} PadusetString;

void PadusetString_Delete(PadusetString* str);
void PadusetString_DeleteTemp(PadusetString str);
void PadusetString_Make(const char* source, PadusetString* out);
void PadusetString_MakeDefault(PadusetString* out);
void PadusetString_Clone(PadusetString* str, PadusetString* out);
void PadusetString_AppendP(PadusetString* str, PadusetString* otherStr);
void PadusetString_AppendC(PadusetString* str, const char* otherStr);
void PadusetString_Assign(PadusetString* target, PadusetString* source);
void PadusetString_FromReal(float real, PadusetString* out);
void PadusetString_FromInteger(int32_t integer, PadusetString* out);
bool PadusetString_EqualsP(PadusetString* lhs, PadusetString* rhs);
bool PadusetString_EqualsC(PadusetString* lhs, const char* rhs);
bool PadusetString_EqualsCC(const char* lhs, const char* rhs);

#ifdef __cplusplus
}
#endif

#endif //PADUSET_PADUSETSTRING_H
