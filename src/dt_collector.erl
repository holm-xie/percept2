-module(dt_collector).

-export([init/1]).

init(Filename) ->
    {ok, File} = file:open(Filename, [write, raw]),
    loop(File).

loop(File) ->
    receive
        start  ->
            S = iof("{ profile_start, ~p }.\n", [erlang:now()]),
            ok = file:write(File, S),
            loop(File);
        stop   ->
            S = iof("{ profile_stop, ~p }.\n", [erlang:now()]),
            ok = file:write(File, S),
            ok = file:close(File);
        {_, TraceMsg} ->
            ok = file:write(File, TraceMsg ++ ".\n"),
            loop(File)
    end.

iof(Fmt, D) -> io_lib:format(Fmt, D).
