#!/bin/bash -eux

### CMAKE OPTIONS
# Ref: http://lists.llvm.org/pipermail/llvm-dev/2016-March/096734.html
# NOTE #3: How to check available cmake-options?
# EXAMPLE #3: cd $BUILD_DIR ; cmake ../llvm -LA | egrep $CMAKE_OPTS
#
# NOTE-1: cmake/ninja: Use LLVM_PARALLEL_COMPILE_JOBS and
# LLVM_PARALLEL_LINK_JOBS options
# NOTE-2: For fast builds use available (online) CPUs +1 or set values
# explicitly
# NOTE-3: For fast and safe linking use bintils-gold and LINK_JOBS="1"
# COMPILE_JOBS="2"

# Some useful docs.
# Purpose of LLVM_ENABLE_LIBCXX: http://lists.llvm.org/pipermail/llvm-dev/2015-July/088689.html

export CC=${CC:-clang}
export CXX=${CXX:-clang++}

usage() {
    echo "usage: ${0} srcdir [name]"
    exit 1
}

if [ "$#" -lt 1 ]; then
    usage
fi

if [ "${1}" == "-h" ] || [ "${1}" == "--help" ]; then
    usage
fi

srcdir=$(readlink -f ${1})

if [ "$#" -ge 2 ]; then
    tag=${2}
else
    timestamp=$(date +'%y%m%d-%H%M%S')
    pushd ${srcdir}
    head=$(git rev-parse HEAD)
    popd
    tag=${head}-${timestamp}
fi
prefix=${HOME}/build/llvm-${tag}
workname=build-${tag}

# make build directory
mkdir ${workname}
cd ${workname}

# echo ">>> Fetching LLVM."
# if [[ ! -d ${srcdir} ]]; then
#     echo "REV: ${REV}"
#     git clone --depth 100 https://github.com/llvm/llvm-project ${srcdir}
# else
#     echo "Skipping clone."
# fi
# if [ ${REV} != "master" ]; then
#     pushd ${srcdir}
#     git checkout ${REV}
#     popd
# fi
# echo ""
# 
echo ">>> Configuring LLVM."

if [ "$(uname)" == "Darwin" ]; then
    projects="clang;clang-tools-extra;compiler-rt;libcxx;libunwind;lld;lldb;openmp;polly"
else
    projects="clang;clang-tools-extra;compiler-rt;libcxx;libcxxabi;lld;lldb;openmp"
fi

cmake_args=(
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_INSTALL_PREFIX=${prefix}
-DCMAKE_C_FLAGS=-march=native
-DCMAKE_CXX_FLAGS=-march=native
-DLLVM_ENABLE_PROJECTS=${projects}
-DLLVM_INSTALL_UTILS=On
-DLLDB_ENABLE_LIBEDIT=On
-DLLVM_ENABLE_ASSERTIONS=Off
-DLIBCXXABI_ENABLE_ASSERTIONS=Off
)
# -DLIBCXX_USE_COMPILER_RT=On
# -DLIBCXXABI_USE_COMPILER_RT=On
# -DLIBCXXABI_USE_LLVM_UNWINDER=On
# -DLIBUNWIND_USE_COMPILER_RT=On
#
# This option is recommended against in the official libc++ documentation
# (https://libcxx.llvm.org/docs/BuildingLibcxx.html), unless libc++abi is
# already installed on the system:
# -DLIBCXX_CXX_ABI=libcxxabi
#
# Cause the Clang/LLVM binaries to be linked against a system-installed libc++:
# -DLLVM_ENABLE_LIBCXX=On
#
# To link libunwind statically into everything add follows:
# -DLIBCXX_ENABLE_STATIC_ABI_LIBRARY=On
# -DLIBCXXABI_ENABLE_STATIC_UNWINDER=On
# -DSANITIZER_USE_STATIC_LLVM_UNWINDER=On

if [ "$(uname)" == "Darwin" ]; then
    cmake_args+=( -DDEFAULT_SYSROOT=$(xcrun --sdk macosx --show-sdk-path))
    cmake_args+=( -DLLVM_TARGETS_TO_BUILD=X86)
    cmake_args+=( -DLLVM_LINK_LLVM_DYLIB=ON)
    cmake_args+=( -DLLVM_ENABLE_LTO=Thin)
    # cmake_args+=( -DCMAKE_EXE_LINKER_FLAGS="-L/Users/stephen/build/llvm/lib -Wl,-rpath /Users/stephen/build/llvm/lib")
    # cmake_args+=( -DLLVM_CREATE_XCODE_TOOLCHAIN=ON)
    cmake_args+=( -DCOMPILER_RT_ENABLE_IOS=Off) # causes compiler-rt to be built with the new Clang
    cmake_args+=( -DLLVM_BUILD_EXTERNAL_COMPILER_RT=ON)
    cmake_args+=( -DLLVM_ENABLE_LIBCXX=ON)
    cmake_args+=( -DLIBUNWIND_ENABLE_ASSERTIONS=Off)
    cmake_args+=( -DLIBCXX_CXX_ABI=libcxxabi)
    cmake_args+=( -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$(xcrun --sdk macosx --show-sdk-path)/usr/include)
    cmake_args+=( -DLIBCXX_CXX_ABI_LIBRARY_PATH=/usr/lib)
    cmake_args+=( -DLLDB_USE_SYSTEM_DEBUGSERVER=ON)
    # cmake_args+=( -DLLDB_INCLUDE_TESTS=OFF) # LLDB tests require libc++
else
    # These args set the Clang's default behavior when building other programs,
    # tend to wreck LLVM tests because there's usually no existing libc++ in
    # the system at the time of the test.
    # cmake_args+=( -DCLANG_DEFAULT_CXX_STDLIB=libc++)
    # cmake_args+=( -DCLANG_DEFAULT_LINKER=lld)
    # cmake_args+=( -DCLANG_DEFAULT_RTLIB=compiler-rt)
    # compiler-rt can replace libgcc_s except that it doesn't incluce an
    # unwinder; that's why we need to specify libunwind as the default
    # unwindlib.
    # See https://github.com/rust-lang/rust/issues/65051#issuecomment-537862559.
    # cmake_args+=( -DCLANG_DEFAULT_UNWINDLIB=libunwind)

    # cmake_args+=( -DCOMPILER_RT_USE_BUILTINS_LIBRARY=On)
    # cmake_args+=( -DCOMPILER_RT_USE_LIBCXX=On)

    # cmake_args+=( -DCMAKE_EXE_LINKER_FLAGS="-L${HOME}/build/llvm/lib -Wl,-rpath,${HOME}/build/llvm/lib")

    cmake_args+=( -DLLVM_ENABLE_LTO=Thin)
    cmake_args+=( -DLLVM_USE_LINKER=lld)

    # cmake_args+=( -DLLVM_PARALLEL_LINK_JOBS=2)

    # not really needed for Void linux
    # cmake_args+=( -DLLVM_LIBDIR_SUFFIX=64)
fi

# echo ">>> Configured arguments:"
# echo ${cmake_args[*]}
cmake ${srcdir}/llvm -G "Ninja" "${cmake_args[@]}"

echo ""
echo ">>> Building."
/usr/bin/time cmake --build . -- -v

echo ""
echo ">>> Checking."
# Tests failing on arwin:
# - check-tsan
# - check-lldb-api
# - check-clang:
#      Clang :: Driver/darwin-header-search-libcxx.cpp
#      Clang :: Driver/darwin-header-search-system.cpp
#      Clang :: Driver/hexagon-toolchain-elf.c
#      Clang :: Driver/mingw-sysroot.cpp
#      Clang :: Driver/mips-cs.cpp
#      Clang :: Driver/mips-fsf.cpp
#      Clang :: Driver/mips-img-v2.cpp
#      Clang :: Driver/mips-img.cpp
#      Clang :: Driver/ps4-header-search.c
#      Clang :: Driver/riscv32-toolchain-extra.c
#      Clang :: Driver/riscv64-toolchain-extra.c
#      Clang :: Driver/sysroot.c
#      Clang :: Frontend/warning-poison-system-directories.c
# Tests failing on Linux (Void):
# - check-clang:
#   Clang :: Driver/cross-linux.c
# - check-sanitizer:
#   SanitizerCommon-asan-x86_64-Linux :: Linux/protoent.cpp
#   SanitizerCommon-lsan-x86_64-Linux :: Linux/protoent.cpp
#   SanitizerCommon-msan-x86_64-Linux :: Linux/protoent.cpp
#   SanitizerCommon-tsan-x86_64-Linux :: Linux/protoent.cpp
#   SanitizerCommon-ubsan-x86_64-Linux :: Linux/protoent.cpp
# - check-msan:
#   MemorySanitizer-Unit :: ./Msan-x86_64-Test/MemorySanitizer.VAArgOverflow
#   MemorySanitizer-Unit :: ./Msan-x86_64-Test/MemorySanitizer.VAArgTest
#   MemorySanitizer-Unit :: ./Msan-x86_64-Test/VectorShiftTest.avx2_left
#   MemorySanitizer-Unit :: ./Msan-x86_64-with-call-Test/MemorySanitizer.VAArgOverflow
#   MemorySanitizer-Unit :: ./Msan-x86_64-with-call-Test/MemorySanitizer.VAArgTest
#   MemorySanitizer-Unit :: ./Msan-x86_64-with-call-Test/VectorShiftTest.avx2_left
# cmake --build . --target check-llvm check-clang check-{asan,msan,tsan} check-cxx check-cxxabi check-lld check-lldb -- -v
/usr/bin/time cmake --build . --target check-all -- -k 0 -v

# echo ""
# echo ">>> Installing."
# cmake --build . --target install

# echo ""
# echo ">>> Creating symbolic link."
# rm -f $HOME/build/llvm
# ln -s $prefix $HOME/build/llvm
# echo ""

echo ">>> Finished!"
