add_library(PadusetRuntime PadusetString.cpp Console.cpp Console.h Core.cpp Core.h)

target_include_directories(PadusetRuntime INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:/>  # <prefix>/include/mylib
        )

if (MSVC)
    target_compile_definitions(PadusetRuntime PRIVATE _CRT_SECURE_NO_WARNINGS=1)
    target_compile_options(PadusetCompiler PUBLIC /EHsc /utf-8)
endif ()

if (UNIX)
    # Compile libstdc++ statically, but not libc.
    # This avoids annoying transitive linking with libstdc++ for final programs.
    target_compile_options(PadusetRuntime PRIVATE -static-libstdc++)
endif()