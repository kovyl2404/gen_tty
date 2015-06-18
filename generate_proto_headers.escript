#! /usr/bin/env escript

-define(_LINE(L), io_lib:format("~s~n",[L])).

main([FileName]) ->
    {ok, Proto} = file:consult(FileName),
    Defines = prepare_defines(Proto, []),
    file:write_file( "c_src/gen_tty_proto.h", generate_c_header(Defines) ),
    file:write_file( "src/gen_tty_proto.hrl", generate_erl_header(Defines) ),
    ok.

prepare_defines([], Acc) ->
    lists:reverse(Acc);
prepare_defines([{Section, Entries} | Rest], Acc) ->
    SectionName = string:to_upper(atom_to_list(Section)),
    prepare_defines(Rest, handle_section_entries(Entries, SectionName, Acc)).

handle_section_entries([], _, Acc) ->
    Acc;
handle_section_entries([{Key, Value} | Rest], SectionName, Acc) ->
    DefineKey = string:to_upper(lists:flatten(io_lib:format("GT_ERL_~s_~p",[SectionName, Key]))),
    NewEnt = {DefineKey, lists:flatten(io_lib:format("~p", [Value]))},
    handle_section_entries(Rest, SectionName, [NewEnt | Acc]).

generate_c_header(Defines) ->
    [
        ?_LINE("#ifndef GEN_TTY_DRV_PROTO_H"),
        ?_LINE("#define GEN_TTY_DRV_PROTO_H"),
        [ io_lib:format("#define ~s ~s~n", [Key, Val])
          || {Key, Val} <- Defines ],
        ?_LINE("#endif")
    ].

generate_erl_header(Defines) ->
    [
        ?_LINE("-ifndef(GEN_TTY_DRV_PROTO_H)."),
        ?_LINE("-define(GEN_TTY_DRV_PROTO_H, ok)."),
        [ io_lib:format("-define(~s, ~s).~n", [Key, Val])
          || {Key, Val} <- Defines ],
        ?_LINE("-endif.")
    ].