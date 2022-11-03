add_executable(PadusetCompiler main.cpp PadusetCompiler.cpp PadusetCompiler.h Tokens.cpp Tokens.h AST.cpp AST.h Parser.cpp Parser.h Linker.cpp Linker.h SemanticAnalyser.cpp SemanticAnalyser.h Diagnostic.cpp StandardLibrary.cpp StandardLibrary.h Options.cpp Options.h)
add_dependencies(PadusetCompiler PadusetRuntime)
if (MSVC)
    target_compile_options(PadusetCompiler PUBLIC /EHsc /utf-8)
    target_compile_definitions(PadusetCompiler PRIVATE ON_WIN32=1)
else ()
    target_compile_definitions(PadusetCompiler PRIVATE ON_WIN32=0)
endif ()
if (UNIX)
    target_compile_definitions(PadusetCompiler PRIVATE UNIX=1 ON_UNIX=1)
else ()
    target_compile_definitions(PadusetCompiler PRIVATE UNIX=0 ON_UNIX=0)
endif ()

add_custom_command(
        TARGET PadusetCompiler
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:PadusetRuntime>
        $<TARGET_FILE_DIR:PadusetCompiler>
)

# Configure LLVM stuff
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(HandleLLVMOptions)
add_definitions(${LLVM_DEFINITIONS})

target_include_directories(PadusetCompiler PRIVATE
        ${LLVM_INCLUDE_DIRS}
        "$<TARGET_PROPERTY:PadusetRuntime,INTERFACE_INCLUDE_DIRECTORIES>")

# Find the libraries that correspond to the LLVM components that we wish to use
llvm_map_components_to_libnames(llvm_libs Core Support Passes X86)

# Link against LLVM libraries and others
target_link_libraries(PadusetCompiler PRIVATE ${llvm_libs} utf8cpp)

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    # Debug symbols are obscenely huge (1 gb!!)
    target_link_options(PadusetCompiler PRIVATE -s)
endif()