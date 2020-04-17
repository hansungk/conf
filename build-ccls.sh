#!/bin/bash -eux
pushd ${HOME}/src/ccls
git pull origin master
git submodule update
popd

CC=clang CXX=clang++ cmake -G Ninja ~/src/ccls -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=$HOME/build/llvm -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld"
cmake --build . -- -v
