#! /bin/bash

mkdir -p c_make
pushd c_make
cmake ..
cmake --build .
popd
mkdir -p priv/
cp ./c_make/gen_tty_drv.so priv/
