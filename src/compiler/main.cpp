#include <iostream>
#include "Tokens.h"
#include "Parser.h"
#include "PadusetCompiler.h"
#include "Linker.h"
#include "SemanticAnalyser.h"
#include "Options.h"

using namespace Paduset;

int main(int argv, char** argc) {
    std::setlocale(LC_ALL, "fr_FR.UTF-8");
    std::setlocale(LC_NUMERIC, "C");

    std::filesystem::path tempOutFile;
    try {
        auto optionsParser = OptionsParser();
        Options options;
        if (!optionsParser.ParseOptions(argc, argv, options)) {
            return 1;
        }

        auto tokenizer = Tokenizer(std::ifstream(options.inputFile));
        auto tokens = tokenizer.Read();
        if (options.verbose) {
            tokenizer.PrintDebugInformation(std::cout);
        }

        auto parser = Parser(tokens);
        auto program = parser.ReadProgramDeclaration();

        // Errors may occur.
        auto analyser = SemanticAnalyser();
        analyser.AnalyseNode(program);

        program.Diagnostics().PrintReport(std::cout);
        if (!program.Success()) {
            std::cerr << "Échec de la compilation.\n";
            return 2;
        }

        auto compiler = PadusetCompiler(program.Node());
        auto result = compiler.CompileIR();
        if (options.verbose) {
            result.module->print(llvm::errs(), nullptr);
        }

        auto fileName = program.Node().Name();

        tempOutFile = options.outputFile; // Copy.
        tempOutFile.replace_extension(".tmp.o");
        auto nativeCode = compiler.CompileNative(result, tempOutFile);

        Linker& link = Linker::Preferred();
        link.Link(nativeCode, options.outputFile);

        // remove(tempOutFile);

        std::cout << "Compilation réussie !\n";
    } catch (const std::exception& e) {
        std::flush(std::cout); // Make sure everything is written down in order.
        std::cerr << "Erreur inattendue : " << e.what() << "\n";

//        if (exists(tempOutFile)) {
//            remove(tempOutFile);
//        }
        return 3;
    }
    return 0;
}
