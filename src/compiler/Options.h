//
// Created by jeuxj on 20/11/2022.
//

#ifndef PADUSET_OPTIONS_H
#define PADUSET_OPTIONS_H

#include <string>
#include <filesystem>

namespace Paduset {
    struct Options {
        std::filesystem::path outputFile{};
        std::filesystem::path inputFile{};
        bool verbose = false;
        bool optimize = false;
        bool run = false;
    };

    class OptionsParser {
    public:
        bool ParseOptions(char** argc, int argv, Options& options);

    private:
        char* NextArg();

        bool MatchArg(const char* value) { return MatchArg(currentArg, value); }

        bool MatchArg(char* arg, const char* value);

        char** args;
        int argCount;
        int currentArgIdx;
        char* currentArg;
    };
}

#endif //PADUSET_OPTIONS_H
