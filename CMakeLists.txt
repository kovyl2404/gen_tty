cmake_minimum_required(VERSION 2.8.4)
project(gen_tty_drv)

set(CMAKE_SHARED_LIBRARY_PREFIX "")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")

execute_process(
    COMMAND erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)])."
    OUTPUT_VARIABLE ERTS_INCLUDE_DIR
)
execute_process(
    COMMAND erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)])."
    OUTPUT_VARIABLE ERL_INTERFACE_INCLUDE_DIR
)
execute_process(
    COMMAND erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)])."
    OUTPUT_VARIABLE ERL_INTERFACE_LIB_DIR
)

include_directories("${ERTS_INCLUDE_DIR}")
include_directories("${ERL_INTERFACE_INCLUDE_DIR}")
link_directories("${ERL_INTERFACE_LIB_DIR}")

if(${WIN32})
    set(IMPL_DIR "win32")
elseif(${UNIX})
    set(IMPL_DIR "linux")
endif()

file(GLOB SOURCE_FILES "c_src/*.c" "c_src/${IMPL_DIR}/*.c")
add_library(gen_tty_drv SHARED ${SOURCE_FILES})
target_link_libraries(gen_tty_drv ei)