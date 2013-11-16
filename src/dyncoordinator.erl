-module(dyncoordinator).

-export([start_profile/3, stop_profile/0, collect/1]).

start_profile(Ns, FN, Opts) ->
    case profile_started() of
        false -> do_start_profile(Ns, FN, Opts);
        true  -> {error, already_started}
    end.

stop_profile() ->
    case profile_started() of
        false -> {error, not_started};
        true  -> do_stop_profile()
    end.

do_start_profile(Ns, FN, Opts) ->
    C = spawn_collector(FN),
    start_collector(C),
    PsOnNs = spawn_profilers(Ns, C, Opts),
    io:format("Done with profilers~n"),
    {PNs, Ps} = lists:unzip(PsOnNs),
    start_profilers(Ps),
    save_profile_details({PNs, Ps, C}),
    {started, PNs}.

do_stop_profile() ->
    {PNs, Ps, C} = retrieve_profile_details(),
    stop_profilers(Ps),
    stop_collector(C),
    delete_profile_details(),
    {stopped, PNs}.

profile_started() ->
    case ets:info(profile_details) of
        undefined -> false;
        _         -> true
    end.

spawn_profilers(Ns, C, Opts) ->
    spawn_profilers(Ns, C, Opts, []).
spawn_profilers([], _C, _Opts, PsOnNs) ->
    PsOnNs;
spawn_profilers([N|Ns], C, Opts, PsOnNs) ->
    case net_adm:ping(N) of
        pong ->
            case rpc:call(N, erlang, system_info, [dynamic_trace]) of
                none -> 
                    spawn_profilers(Ns, C, Opts, PsOnNs);
                _    ->          
                    P = spawn(N, dynprofiler, run, [C, Opts]),
                    spawn_profilers(Ns, C, Opts, [{N, P} | PsOnNs])
            end;
        pang ->
            spawn_profilers(Ns, C, Opts, PsOnNs)
    end.

spawn_collector(FN) ->
    spawn(?MODULE, collect, [FN]).

collect(FN) ->
    {ok, F} = file:open(FN, [write, raw]),
    loop(F).

loop(F) ->
    receive
        start  ->
            S = iof("{ profile_start, ~p }.\n", [erlang:now()]),
            ok = file:write(F, S),
            loop(F);
        stop   ->
            S = iof("{ profile_stop, ~p }.\n", [erlang:now()]),
            ok = file:write(F, S),
            ok = file:close(F);
        {_, TraceMsg} ->
            ok = file:write(F, TraceMsg ++ ".\n"),
            loop(F)
    end.

start_profilers(Ps) ->
    lists:foreach(fun(P) -> P ! start end, Ps).

start_collector(C) ->
    C ! start.

stop_profilers(Ps) ->
    lists:foreach(fun(P) -> P ! stop end, Ps).

stop_collector(C) ->
    C ! stop.

save_profile_details({PNs, Ps, C}) ->
    ets:new(profile_details, [set, public, named_table]),
    true = ets:insert(profile_details, {nodes, PNs}),
    true = ets:insert(profile_details, {profilers, Ps}),
    true = ets:insert(profile_details, {collector, C}).

delete_profile_details() ->
    true = ets:delete(profile_details).

retrieve_profile_details() ->
    [{nodes, PNs}] = ets:lookup(profile_details, nodes),
    [{profilers, Ps}] = ets:lookup(profile_details, profilers),
    [{collector, C}] = ets:lookup(profile_details, collector),
    {PNs, Ps, C}.

iof(Fmt, D) -> io_lib:format(Fmt, D).
