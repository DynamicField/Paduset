cmake_minimum_required(VERSION 3.20)
set(CMAKE_TOOLCHAIN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake"
        CACHE STRING "Vcpkg toolchain file")

project(Paduset)

find_package(LLVM CONFIG REQUIRED)
find_package(utf8cpp CONFIG REQUIRED)

set(CMAKE_CXX_STANDARD 17)

add_subdirectory(src/compiler)
add_subdirectory(src/runtime)