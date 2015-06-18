
-module(gen_tty_SUITE).
-author("Viacheslav V. Kovalev").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    open_non_existent_file/1,
    open_not_permitted_file/1,
    open_regular_file/1,
    get_set_opts/1,
    set_bad_speed/1
]).

all() ->
    [
        open_non_existent_file,
        open_not_permitted_file,
        open_regular_file,
        get_set_opts,
        set_bad_speed
    ].

init_per_suite(Config) ->
    ok = application:start(gen_tty),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(gen_tty).

open_non_existent_file(Config) ->
    FileName = gen_tty_test_utils:random_file_name(Config),
    {error, enoent} = gen_tty:open(FileName).

open_not_permitted_file(Config) ->
    FileName = gen_tty_test_utils:not_permitted_file_name(Config),
    {error, eacces} = gen_tty:open(FileName).

open_regular_file(Config) ->
    FileName = gen_tty_test_utils:random_file_name(Config),
    file:write_file(FileName, "WATSUP"),
    {error, enotty} = gen_tty:open(FileName).

get_set_opts(Config) ->
    {ok, FileName, _} = gen_tty_test_utils:tty_pair(),
    {ok, Port1} = gen_tty:open(FileName),
    Opts0 = gen_tty:getopts(Port1),

    none = proplists:get_value(parity, Opts0),
    8 = proplists:get_value(csize, Opts0),
    38400 = proplists:get_value(speed, Opts0),

    ok = gen_tty:setopts(Port1, [{speed, 4800}, {csize, 5}, {parity, even}]),
    Opts1 = gen_tty:getopts(Port1),
    even = proplists:get_value(parity, Opts1),
    5 = proplists:get_value(csize, Opts1),
    4800 = proplists:get_value(speed, Opts1),

    ok = gen_tty:close(Port1),
    {ok, Port2} = gen_tty:open(FileName),
    Opts0 = gen_tty:getopts(Port2),

    {error, einval} = gen_tty:setopts(Port2, [{csize, 3}]),
    Opts0 = gen_tty:getopts(Port2),
    gen_tty:close(Port2).

set_bad_speed(Config) ->
    {ok, FileName, _} = gen_tty_test_utils:tty_pair(),
    {ok, Port1} = gen_tty:open(FileName),
    Opts0 = gen_tty:getopts(Port1),
    {error, einval} = gen_tty:setopts(Port1, [{speed, 4900}]),
    Opts0 = gen_tty:getopts(Port1),
    gen_tty:close(Port1).
