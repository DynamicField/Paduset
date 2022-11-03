//
// Created by jeuxj on 31/10/2022.
//

#ifndef PADUSET_LINKER_H
#define PADUSET_LINKER_H

#include <filesystem>
#include "PadusetCompiler.h"

namespace Paduset {
    // Currently, amd64 only.
    class Linker {
    public:
        Linker() = default;

        virtual ~Linker() = default;

        bool Usable();

        void Link(const NativeCompiledProgram& compiled,
                  const std::filesystem::path& outputPath);

        virtual const std::string& Name() = 0;

        static Linker& Preferred();

    protected:
        static bool FindFileInSystemPath(const std::string& fileName, std::filesystem::path& foundPath);

        virtual bool FindLinkerExecutable(std::string& executable) = 0;

        virtual void LinkProgram(const NativeCompiledProgram& compiled,
                                 const std::filesystem::path& outputPath,
                                 const std::string& linkerExecutable) = 0;

        static const std::string& PadusetRuntimeLibPath() {
            return padusetRuntimeLibPath;
        }

    private:
        static std::string FindPadusetRuntimeLib();

        std::string linkerExecutable{};
        bool findAttempted = false;
        bool linkerFound = false;

        static std::string padusetRuntimeLibPath;
        static std::unique_ptr<Linker> preferredLinker;
    };

    class CCLinker : public Linker {
    public:
        CCLinker() = default;

        const std::string& Name() override;

    protected:
        bool FindLinkerExecutable(std::string& executable) override;

        void LinkProgram(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath,
                         const std::string& linkerExecutable) override;
    };

    class MSVCLinker : public Linker {
    public:
        MSVCLinker() = default;

        const std::string& Name() override;

    protected:
        bool FindLinkerExecutable(std::string& executable) override;

        void LinkProgram(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath,
                         const std::string& linkerExecutable) override;

    private:
        std::string libPath{};
        std::string um{};
        std::string ucrt{};
    };
};

#endif //PADUSET_LINKER_H
