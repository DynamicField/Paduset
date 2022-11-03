//
// Created by jeuxj on 31/10/2022.
//

#include "Linker.h"

#if ON_WIN32

#include <Windows.h>
#include <strsafe.h>

#elif ON_UNIX

#include <unistd.h>
#include <sys/wait.h>

#endif

#include "utf8.h"

using namespace Paduset;

bool Linker::Usable() {
    if (!findAttempted) {
        linkerFound = FindLinkerExecutable(linkerExecutable);
        findAttempted = true;
    }
    return linkerFound;
}

void Linker::Link(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath) {
    if (!Usable()) {
        throw std::runtime_error("Unusable linker.");
    }
    LinkProgram(compiled, outputPath, linkerExecutable);
}

bool Linker::FindFileInSystemPath(const std::string& fileName, std::filesystem::path& foundPath) {
    const std::string path = std::getenv("PATH");
#if ON_WIN32
    const char separator = ';';
#else
    const char separator = ':';
#endif

    const auto search = [&](std::size_t start, std::size_t end) -> std::filesystem::path {
        const std::string part = path.substr(start, end - start);
        auto fullFilePath = std::filesystem::path(part) / fileName;

        if (std::filesystem::exists(fullFilePath)) {
            return fullFilePath;
        } else {
            return {}; // Empty.
        }
    };

    std::size_t start = 0, end = 0;
    while (end != std::string::npos) {
        if (start != end) {
            auto found = search(start, end);
            if (!found.empty()) {
                foundPath = absolute(found);
                return true;
            }
            start = end + 1;
        }
        end = path.find(separator, start);
    }

    return false;
}

std::unique_ptr<Linker> Linker::preferredLinker{};
std::string Linker::padusetRuntimeLibPath = FindPadusetRuntimeLib();

Linker& Linker::Preferred() {
    auto const tryLinker = [&](Linker* linker) -> Linker* {
        if (linker->Usable()) {
            preferredLinker = std::unique_ptr<Linker>(linker);
            return linker;
        } else {
            delete linker;
            return nullptr;
        }
    };

    if (padusetRuntimeLibPath.empty()) {
        throw std::runtime_error("Couldn't find the PadusetRuntime library.");
    }

    if (preferredLinker) {
        return *preferredLinker;
    } else if (auto* cc = tryLinker(new CCLinker())) {
        return *cc;
    } else if (auto* msvc = tryLinker(new MSVCLinker())) {
        return *msvc;
    } else {
        throw std::runtime_error("Couldn't find a compatible linker.");
    }
}

std::string Linker::FindPadusetRuntimeLib() {
#if ON_WIN32
    TCHAR pathStr[1024]; // Support long paths
    if (!GetModuleFileName(nullptr, pathStr, 1024)) {
        throw std::runtime_error("Cannot retrieve executable path.");
    }

    std::filesystem::path path{pathStr};
#elif ON_UNIX
    char pathStr[PATH_MAX];
    ssize_t charsRead = readlink("/proc/self/exe", pathStr, PATH_MAX);
    if (charsRead == -1) {
        throw std::runtime_error("Cannot retrieve executable path.");
    }

    std::filesystem::path path{std::string(pathStr, charsRead)};
#endif

    path = path.parent_path(); // Remove the file name.

#if ON_WIN32
    std::filesystem::path padusetLib = path / "PadusetRuntime.lib";
#elif ON_UNIX
    std::filesystem::path padusetLib = path / "libPadusetRuntime.a";
#endif

    if (exists(padusetLib)) {
        return absolute(padusetLib).string();
    } else {
        return "";
    }
}


// MSVC stuff
// Did you know? Finding VS dev tools stuff is a CHORE compared to linux.

bool FindVisualStudioToolchain(std::string* linkerExe, std::string* lib, std::string* um, std::string* ucrt) {
    // On my machine(tm):
    // C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.33.31629\bin\Hostx64\x64\link.exe
    // Also, FUN FACT! The by-default linker in the Developer Command Prompt is the x86 (32 bit) one.

    std::filesystem::path vsPath("C:\\Program Files\\Microsoft Visual Studio");
    if (!std::filesystem::exists(vsPath)) {
        return false;
    }

    int foundVer = 0;
    const char* foundName;

    for (auto const& entry: std::filesystem::directory_iterator(vsPath)) {
        if (entry.is_directory()) {
            const std::string& dirName = entry.path().filename().string();
            if (dirName == "2022") {
                foundVer = 2022;
                foundName = "2022";
                break; // Most recent.
            } else if (dirName == "2019" && foundVer < 2019) {
                foundVer = 2019;
                foundName = "2019";
            } else if (dirName == "2016" && foundVer < 2016) {
                foundVer = 2016;
                foundName = "2016";
            }
        }
    }

    if (foundVer == 0) {
        // Give up.
        return false;
    }

    std::filesystem::path versionedPath = vsPath / foundName;
    std::filesystem::path editionPath;
    for (const auto& editionEntry: std::filesystem::directory_iterator(versionedPath)) {
        if (editionEntry.is_directory()) {
            const std::string& dirName = editionEntry.path().filename().string();
            if (dirName == "Community" || dirName == "Enterprise" || dirName == "Professional") {
                editionPath = editionEntry.path();
                break;
            }
        }
    }

    if (editionPath.empty()) {
        return false;
    }

    std::filesystem::path msvcDirectory = editionPath / "VC" / "Tools" / "MSVC";
    if (!std::filesystem::exists(msvcDirectory)) {
        return false;
    }

    std::filesystem::path sdkDirectory;
    for (const auto& sdkEntry: std::filesystem::directory_iterator(msvcDirectory)) {
        if (sdkEntry.is_directory()) {
            // Just pick the first one. We could choose the most recent SDK
            // but honestly parsing the version number and sorting all of them
            // just sounds boring so let's not.
            sdkDirectory = sdkEntry.path();
            break;
        }
    }
    if (sdkDirectory.empty()) {
        return false;
    }

    auto linkerPath = sdkDirectory / "bin" / "Hostx64" / "x64" / "link.exe";
    if (!std::filesystem::exists(linkerPath)) {
        return false; // How???
    }

    *lib = absolute(sdkDirectory / "lib" / "x64").string();
    *linkerExe = absolute(linkerPath).string();

    std::filesystem::path windowsKitsPath{R"(C:\Program Files (x86)\Windows Kits\10\Lib)"};
    if (!exists(windowsKitsPath)) {
        return false;
    }

    // "C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\um\\x64"
    // "C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\ucrt\\x64"

    auto winKitIt = std::filesystem::directory_iterator(windowsKitsPath);
    if (winKitIt == std::filesystem::end(winKitIt)) {
        return false;
    }

    // Don't really care about the most recent one.
    std::filesystem::path versionedWinKitPath = winKitIt->path();
    std::filesystem::path umPath = versionedWinKitPath / "um" / "x64";
    std::filesystem::path ucrtPath = versionedWinKitPath / "ucrt" / "x64";

    assert(exists(umPath) && exists(ucrtPath));

    *um = absolute(umPath).string();
    *ucrt = absolute(ucrtPath).string();

    return true;
}

bool MSVCLinker::FindLinkerExecutable(std::string& executable) {
#if ON_WIN32
    return FindVisualStudioToolchain(&executable, &libPath, &um, &ucrt);
#else
    return false;
#endif
}

void MSVCLinker::LinkProgram(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath,
                             const std::string& linkerExecutable) {
#if ON_WIN32
    // MSVC Linker syntax (for gcc peasants):
    // link.exe myfile.o /ENTRY:main /OUT:myfile.exe x.lib y.lib
    // is equivalent to
    // gcc mylife.o -o myfile
    // and the libs are there because link.exe is too dumb to find the library itself.
    std::string commandLine = linkerExecutable +
                              " \"" + compiled.objectFile.string() + '"' +
                              " \"" + PadusetRuntimeLibPath() + '"' +
                              " /SUBSYSTEM:CONSOLE" +
                              " \"/OUT:" + absolute(outputPath).string() + '"' +
                              " \"/libpath:" + libPath + '"' +
                              " \"/libpath:" + um + '"' +
                              " \"/libpath:" + ucrt + '"' +
                              " /debug";

    std::u16string wideCommandLine = utf8::utf8to16(commandLine);
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION processInfo;
    ZeroMemory(&startupInfo, sizeof(STARTUPINFO));
    ZeroMemory(&processInfo, sizeof(PROCESS_INFORMATION));
    startupInfo.cb = sizeof(STARTUPINFO); // I have no idea what this does

    if (CreateProcessW(nullptr, (LPWSTR) wideCommandLine.c_str(), nullptr, nullptr, false, 0, nullptr, nullptr,
                       &startupInfo, &processInfo)) {
        WaitForSingleObject(processInfo.hProcess, INFINITE);

        DWORD exitCode = 0xAAAA; // Default value in case GetExitCode doesn't work.
        GetExitCodeProcess(processInfo.hProcess, &exitCode);

        CloseHandle(processInfo.hProcess);
        CloseHandle(processInfo.hThread);

        if (exitCode != 0) {
            throw std::runtime_error("Unexpected linker exit code.");
        }
    }
#endif
}

const std::string& MSVCLinker::Name() {
    static const std::string name = "MSVC";
    return name;
}

//void LLDLinker::LinkProgram(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath,
//                            const std::string& linkerExecutable) {
//
//    // OF COURSE, I forgot to put the first parameter... which is apparently important
//    // because main args are weird.
//#if WIN32
//    bool success = lld::coff::link({"lld-link",
//                                    compiled.objectFile.string().c_str(),
//                                    Linker::PadusetRuntimeLibPath().c_str(),
//                                    ("/out:" + absolute(outputPath).string()).c_str(),
//                                    ("/libpath:" + libPath).c_str(),
//                                    "/subsystem:console",
//                                    "/debug",
//                                    "/machine:x64",
//                                    "/verbose"},
//                                   llvm::outs(), llvm::errs(), false, false);
//#elif UNIX
//    std::filesystem::path gccToolchain{"/usr/lib/gcc/x86_64-linux-gnu"};
//    auto versionedLibPath = (*std::filesystem::directory_iterator(gccToolchain)).path().string();
//
//    auto outPath = absolute(outputPath).string();
//    std::cout << outPath << std::endl;
//    bool success = lld::elf::link({"ld.lld",
//                                   compiled.objectFile.string().c_str(),
//                                   Linker::PadusetRuntimeLibPath().c_str(),
//                                   "-o",
//                                   outPath.c_str(),
//                                   "-lc",
//                                   "-lstdc++",
//                                   "-lstdc++fs",
//                                   "-lgcc",
//                                   "-lgcc_eh",
//                                   "-lgcc_s",
//                                   "-v",
//                                   "--entry",
//                                   "main",
//                                   "-L/usr/lib/x86_64-linux-gnu/",
//                                   ("-L" + versionedLibPath).c_str()},
//                                  llvm::outs(), llvm::errs(), false, false);
//#endif
//
//    if (!success) {
//        throw std::runtime_error("Linking error.");
//    }
//}
const std::string& CCLinker::Name() {
    static const std::string name = "cc";
    return name;
}

bool CCLinker::FindLinkerExecutable(std::string& executable) {
    std::filesystem::path path;
    if (FindFileInSystemPath("cc", path)) {
        executable = path.string();
        return true;
    } else {
        return false;
    }
}

void CCLinker::LinkProgram(const NativeCompiledProgram& compiled, const std::filesystem::path& outputPath,
                           const std::string& linkerExecutable) {
#if ON_UNIX
    pid_t pid = fork();
    if (pid == 0) {
        // Child process: DO THE THING!

        // gcc paduset_compiled.o libPadusetRuntime.a -o outputExe -no-pie -g -lstdc++
        // Note: I shouldn't have to link stdc++, but the PadusetRuntime cmake is a bit broken.
        const char* args[] = {
                linkerExecutable.c_str(),
                compiled.objectFile.c_str(),
                Linker::PadusetRuntimeLibPath().c_str(),
                "-o",
                outputPath.c_str(),
                "-no-pie", // Avoids some issues with PIE enabled by default. Should maybe add PIE support one day.
                "-g",
                "-lstdc++",
                nullptr // Of course
        };
        int result = execve(linkerExecutable.c_str(), const_cast<char**>(args), environ);
        std::cerr << "What the hell??? Unexpected execve value: " << result << std::endl; // Shouldn't continue there!
        exit(1);
    } else {
        // Current process: WAIT FOR THE THING!
        int stat;
        pid_t result = wait(&stat);
        if (result == -1 || stat != 0) {
            throw std::runtime_error("Linking failed!");
        }
    }
#endif
}
