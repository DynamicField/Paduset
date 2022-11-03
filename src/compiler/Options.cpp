//
// Created by jeuxj on 20/11/2022.
//

#include "Options.h"
#include <cassert>
#include <cstring>
#include <filesystem>
#include <iostream>

using namespace Paduset;

bool OptionsParser::ParseOptions(char** argc, int argv, Options& options) {
    args = argc;
    argCount = argv;
    currentArgIdx = 0; // This is intended: -1 -> 0 at first iteration (wtf)
    currentArg = nullptr;

    bool somethingWentWrong = false;

    // We skip the first argument
    while (char* arg = NextArg()) {
        if (MatchArg("--output") || MatchArg("-o")) {
            if (const char* outPathStr = NextArg()) {
                options.outputFile = absolute(std::filesystem::path(outPathStr));
            }

            if (options.outputFile.empty()) {
                std::cout << "Chemin valide attendu après '" << arg << "'\n";
                somethingWentWrong = true;
            }
        }
        else if (MatchArg("--verbose") || MatchArg("-v")) {
            options.verbose = true;
        }
        else if (MatchArg("--optimize") || MatchArg("-O")) {
            options.optimize = true;
        }
        else if (MatchArg("--run") || MatchArg("-r")) {
            options.run = true;
        }
        else if (*currentArg == '-') { // First char
            std::cout << "Option inconnue : '" << currentArg << "'\n";
            somethingWentWrong = true;
        } else if (options.inputFile.empty()) {
            // Treat it as the input file
            std::filesystem::path inPath{currentArg};
            if (exists(inPath)) {
                options.inputFile = absolute(inPath);
            } else {
                std::cout << "Fichier introuvable : " << inPath.string() << "\n";
            }
        } else {
            std::cout << "Fichier déjà spécifié.\n";
            somethingWentWrong = true;
        }
    }

    if (options.inputFile.empty()) {
        std::cout << "Aucun fichier à compiler donné.\n";
        somethingWentWrong = true;
    }

    if (options.outputFile.empty()) {
        std::cout << "Aucun exécutable de sortie donné.\n";
        somethingWentWrong = true;
    }

    return !somethingWentWrong;
}

char* OptionsParser::NextArg() {
    assert(currentArgIdx < argCount);

    if (currentArgIdx == argCount) {
        return nullptr;
    } else {
        currentArgIdx++;
        currentArg = args[currentArgIdx];
        return currentArg;
    }
}

// Do a case-insensitive match.
bool OptionsParser::MatchArg(char* arg, const char* value) {
#if ON_UNIX
    return strcasecmp(arg, value) == 0;
#elif ON_WIN32
    return _stricmp(arg, value) == 0;
#else
#error Unsupported OS.
#endif
}
