//
// Created by jeuxj on 03/11/2022.
//

#include <cstdlib>
#include <cstring>
#include <charconv>
#include <cassert>
#include "PadusetString.h"

#ifdef _MSC_VER
#include <intrin.h>
#include <cstdio>
#include <iostream>

#endif

// Example usage:
//
//    PadusetString truc;
//    PadusetString_MakeDefault(&truc);
//
//    PadusetString lit;
//    PadusetString_Make("coucou c'est ", &lit);
//
//    PadusetString int_to_str;
//    PadusetString_FromInteger(15, &int_to_str);
//    PadusetString_AppendP(&lit, &int_to_str);
//    PadusetString_Assign(&truc, &lit);
//    PadusetString_Delete(&lit);
//
//    std::cout << truc.buffer << "\n";
//
//    return 0;

int32_t NextBufferSize(int32_t current) {
    return current << 1;
}

int32_t RoundUpBufferSize(int32_t size) {
#if _MSC_VER
    unsigned long mostSignificantBitPos = 0;
    // BitScanReverse gives the position (starting by 0) of the most significant bit.
    // Example: 0b00001110 (14) returns 3
    _BitScanReverse(&mostSignificantBitPos, static_cast<unsigned long>(size));
    return 1 << static_cast<int32_t>(mostSignificantBitPos + 1);
#elif __GNUC__
    return 1 << ((32 - __builtin_clz(size)) + 1);
#else
    return 1 << (((int)log2f((float)size) + 1) + 1); // Not very good but heh it's portable
#endif
}

void Resize(PadusetString* str, int32_t minimum = -1) {
    int32_t next;
    if (minimum == -1) {
        next = NextBufferSize(str->bufferSize);
    } else {
        next = str->bufferSize;
        while (next < minimum) {
            next = NextBufferSize(str->bufferSize);
        }
    }
    str->buffer = (char*) std::realloc(str->buffer, next);
    str->bufferSize = next;
}

void PadusetString_Delete(PadusetString* str) {
    if (str->length != -1) {
        std::free(str->buffer);
        str->length = -1;
        str->buffer = nullptr;
    }
}

void PadusetString_DeleteTemp(PadusetString str) {
    if (str.length != -1) {
        free(str.buffer);
    }
}

void PadusetString_Make(const char* source, PadusetString* out) {
    auto length = static_cast<int32_t>(std::strlen(source));
    auto sourceBufferSize = length + 1;
    auto roundedUpSize = RoundUpBufferSize(sourceBufferSize);
    if (roundedUpSize < 8) {
        // Have a minimum size (64-bit like) to avoid excessive re-allocations.
        roundedUpSize = 8;
    }

    out->buffer = (char*) std::malloc(roundedUpSize);
    out->length = length;
    out->bufferSize = roundedUpSize;

    std::memcpy(out->buffer, source, out->bufferSize);
}

void PadusetString_AppendP(PadusetString* str, PadusetString* otherStr) {
    if (otherStr->length <= 0) {
        // Empty again.
        return;
    }

    int32_t requiredBufSize = str->length + otherStr->length + 1;
    if (requiredBufSize > str->bufferSize) {
        Resize(str, requiredBufSize);
    }

    const char* otherCursor = otherStr->buffer;
    char* ourCursor = str->buffer + str->length;
    str->length += otherStr->length;

    // Same thing
    while (*otherCursor != '\0') {
        *ourCursor = *otherCursor;
        ourCursor++;
        otherCursor++;
    }

    // Restore the null char.
    *ourCursor = '\0';
}

void PadusetString_AppendC(PadusetString* str, const char* otherStr) {
    if (*otherStr == '\0') {
        // Empty, don't bother.
        return;
    }

    const char* otherCursor = otherStr;
    char* ourCursor = str->buffer + str->length;

    // We're going to overwrite the ending NUL char temporarily.
    while (*otherCursor != '\0') {
        // For instance, if our length is 16, and our bufferSize is 16,
        // we won't have enough space for the NUL char.
        str->length++; // Increment the length now to get the right amount of current chars.
        if (str->length >= str->bufferSize) {
            Resize(str);
            ourCursor = str->buffer + str->length - 1; // Rewind 1 character back to put the new one correctly.
        }
        *ourCursor = *otherCursor;

        ourCursor++;
        otherCursor++;

    }
    // Restore the null char.
    *ourCursor = '\0';
}

void PadusetString_Clone(PadusetString* str, PadusetString* out) {
    out->buffer = (char*) malloc(str->bufferSize);
    std::memcpy(out->buffer, str->buffer, str->bufferSize);
    out->bufferSize = str->bufferSize;
    out->length = str->length;
}

template<typename T>
void MakeUsingToChars(T value, PadusetString* str) {
    str->buffer = (char*) malloc(32);
    str->bufferSize = 32;
    str->length = 0;

    std::to_chars_result result{};
    do {
        result = std::to_chars(str->buffer, str->buffer + str->bufferSize - 1, value);
        if (result.ec == std::errc()) {
            // Success!
            *result.ptr = '\0'; // one-past-the-end pointer of the characters written
            str->length = static_cast<int32_t>(result.ptr - str->buffer);
        } else if (result.ec == std::errc::value_too_large) {
            Resize(str);
        } else {
            break; // Well that's sad
        }
    } while (result.ec == std::errc::value_too_large && str->bufferSize <= 512);

    assert(str->length != 0);
}

void PadusetString_FromReal(float real, PadusetString* out) {
    MakeUsingToChars(real, out);
}

void PadusetString_FromInteger(int32_t integer, PadusetString* out) {
    MakeUsingToChars(integer, out);
}

void PadusetString_Assign(PadusetString* target, PadusetString* source) {
    if (target->length > source->length) {
        std::memset(target->buffer, 0, source->length + 1);
    }
    Resize(target, source->length);
    std::memcpy(target->buffer, source->buffer, source->length + 1);
}

void PadusetString_MakeDefault(PadusetString* out) {
    out->buffer = (char*) std::malloc(16);
    out->buffer[0] = '\0';
    out->length = 0;
    out->bufferSize = 16;
}

bool PadusetString_EqualsP(PadusetString* lhs, PadusetString* rhs) {
    if (lhs->length != rhs->length) {
        return false;
    }
    return (std::strcmp(lhs->buffer, rhs->buffer) == 0);
}

bool PadusetString_EqualsC(PadusetString* lhs, const char* rhs) {
    return (std::strcmp(lhs->buffer, rhs) == 0);
}

bool PadusetString_EqualsCC(const char* lhs, const char* rhs) {
    return (std::strcmp(lhs, rhs) == 0);
}
