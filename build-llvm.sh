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

export CC=${CC:-/usr/bin/clang}
export CXX=${CXX:-/usr/bin/clang++}
export CFLAGS='-march=native -O2 -DNDEBUG'
export CXXFLAGS=${CFLAGS}
prefix=$HOME/build/llvm-$(date +'%y%m%d')
srcdir=$HOME/src/llvm-project

cmake_args=(
-DCMAKE_BUILD_TYPE=Release
-DCMAKE_INSTALL_PREFIX=${prefix}
-DCMAKE_C_FLAGS_RELEASE=
-DCMAKE_CXX_FLAGS_RELEASE=
-DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;compiler-rt;libcxx;libcxxabi;libunwind;lld;lldb;openmp;polly"
-DLLVM_INSTALL_UTILS=On
-DLIBCXX_CXX_ABI=libcxxabi
-DLIBCXX_USE_COMPILER_RT=On
-DLIBCXXABI_USE_COMPILER_RT=On
-DLIBCXXABI_USE_LLVM_UNWINDER=On
-DLIBCXXABI_ENABLE_ASSERTIONS=Off
-DLIBUNWIND_USE_COMPILER_RT=On
-DLIBUNWIND_ENABLE_ASSERTIONS=Off
-DLLDB_ENABLE_LIBEDIT=On
)
# -DLLVM_ENABLE_LIBCXX=On
# To link libunwind statically into everything add follows:
# -DLIBCXX_ENABLE_STATIC_ABI_LIBRARY=On
# -DLIBCXXABI_ENABLE_STATIC_UNWINDER=On
# -DSANITIZER_USE_STATIC_LLVM_UNWINDER=On

if [ "$(uname)" == "Darwin" ]; then
    cmake_args+=( -DLLVM_CREATE_XCODE_TOOLCHAIN=ON)
    cmake_args+=( -DCOMPILER_RT_ENABLE_IOS=Off)
    cmake_args+=( -DLLVM_BUILD_EXTERNAL_COMPILER_RT=ON)
    cmake_args+=( -DLLVM_ENABLE_LIBCXX=ON)
    cmake_args+=( -DDEFAULT_SYSROOT=$(xcrun --sdk macosx --show-sdk-path))
else
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
    cmake_args+=( -DLLVM_USE_LINKER=gold)
    cmake_args+=( -DLLVM_PARALLEL_LINK_JOBS=1)
    # not really needed for Void linux
    # cmake_args+=( -DLLVM_LIBDIR_SUFFIX=64)
fi

cmake $srcdir/llvm -G Ninja ${cmake_args[@]}

ninja

ninja check-all

ninja install

rm -f $HOME/build/llvm
ln -s $prefix $HOME/build/llvm
