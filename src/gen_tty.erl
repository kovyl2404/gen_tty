-module(gen_tty).
-author("Viacheslav V. Kovalev").

-include("gen_tty_proto.hrl").

-behaviour(application).

%% Interface functions
-export([
    open/1,
    close/1,
    getopts/1,
    setopts/2
]).

%% Application callbacks
-export([start/2,
  stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(DRIVER_NAME, "gen_tty_drv").

%%%===================================================================
%%% Interface functions
%%%===================================================================

open(FileName) when is_list(FileName) ->
    try erlang:open_port({spawn, ?DRIVER_NAME ++ " " ++ FileName}, []) of
        Port -> {ok, Port}
    catch error:What ->
        {error, What}
    end.

close(Port) ->
    erlang:port_close(Port),
    ok.

getopts(Port) ->
    try erlang:port_control(Port, ?GT_ERL_CTL_GETOPTS, "") of
        Binary -> translate_opts(binary_to_term(Binary))
    catch _:Reason ->
        {error, Reason}
    end.

setopts(Port, Opts) ->
    RawOpts = translate_opts(Opts, []),
    try erlang:port_control(Port, ?GT_ERL_CTL_SETOPTS, term_to_binary(RawOpts)) of
        Binary -> binary_to_term(Binary)
    catch _:Reason ->
        {error, Reason}
    end.

translate_opts(Opts) ->
    translate_opts(Opts, []).

translate_opts([], Acc) ->
    Acc;
translate_opts([Opt | Rest], Acc) ->
    translate_opts(Rest, [translate_opt(Opt) | Acc]).

translate_opt({?GT_ERL_OPT_SPEED, Value}) ->
    {speed, Value};
translate_opt({speed, Value}) ->
    {?GT_ERL_OPT_SPEED, Value};
translate_opt({?GT_ERL_OPT_CSIZE, Value}) ->
    {csize, Value};
translate_opt({csize, Value}) ->
    {?GT_ERL_OPT_CSIZE, Value};
translate_opt({?GT_ERL_OPT_PARITY, Value}) ->
    {parity, translate_parity(Value)};
translate_opt({parity, Value}) ->
    {?GT_ERL_OPT_PARITY, translate_parity(Value)}.

translate_parity(?GT_ERL_PARITY_VAL_NONE) -> none;
translate_parity(?GT_ERL_PARITY_VAL_EVEN) -> even;
translate_parity(?GT_ERL_PARITY_VAL_ODD) -> odd;
translate_parity(?GT_ERL_PARITY_VAL_MARK) -> mark;
translate_parity(?GT_ERL_PARITY_VAL_SPACE) -> space;

translate_parity(none) -> ?GT_ERL_PARITY_VAL_NONE;
translate_parity(even) -> ?GT_ERL_PARITY_VAL_EVEN;
translate_parity(odd) -> ?GT_ERL_PARITY_VAL_ODD;
translate_parity(mark) -> ?GT_ERL_PARITY_VAL_MARK;
translate_parity(space) -> ?GT_ERL_PARITY_VAL_SPACE.


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case erl_ddll:load(code:priv_dir(?MODULE), ?DRIVER_NAME) of
        ok ->
            start_top_supervisor();
        {error, Reason} ->
            {error, erl_ddll:format_error(Reason)}
    end.


stop(_State) ->
    case erl_ddll:unload(?DRIVER_NAME) of
        ok -> ok;
        {error, Reason} ->
            {error, erl_ddll:format_error(Reason)}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_) ->
    {ok, {{one_for_one, 1, 1000}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


start_top_supervisor() ->
    supervisor:start_link(?MODULE, []).