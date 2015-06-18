-module(gen_tty_test_utils).
-author("Viacheslav V. Kovalev").

%% API
-export([
    random_file_name/1,
    not_permitted_file_name/1,
    tty_pair/0
]).

random_file_name(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    {_, _, Now} = erlang:now(),
    filename:nativename(filename:join(PrivDir, integer_to_list(Now))).

not_permitted_file_name(Config) ->
    {unix, _} = os:type(),
    "/etc/shadow".

tty_pair() ->
    {unix, _} = os:type(),
    {ok, "/dev/nmp0", "/dev/nmp1"}.