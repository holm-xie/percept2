-module(dt_analyzer).

-export([analyze/1, parse/3]).

-include("../include/percept2.hrl").

analyze(SubDBs) ->
    Parsers = lists:foldl(fun(SubDB, Pids) ->
                              Pid = start_parser(SubDB),
                              [Pid | Pids]
                          end, [], SubDBs),
    wait_for_parsers(Parsers).

start_parser({Filename, SubDB}) ->
    Pid = spawn_link(?MODULE, parse, [Filename, SubDB, self()]),
    receive
        {started, {Filename, SubDB}} -> Pid
    end.

wait_for_parsers(Parsers) ->
    receive
        {Pid, done} ->
            Pid ! {ack, self()},
            case Parsers -- [Pid] of
                [] ->
                    try percept2_db:consolidate_db()
                    catch _E1:_E2 -> ok
                    end,
                    io:format("    ~p created processes.~n",
                              [percept2_db:select({information, procs_count})]),
                    io:format("    ~p opened ports.~n",
                              [percept2_db:select({information, ports_count})]);
                ParsersLeft ->
                    wait_for_parsers(ParsersLeft)
            end;
        {error, Reason} ->
            percept2_db:stop(percept2_db),
            flush(),
            {error, Reason};
        _Other ->
            wait_for_parsers(Parsers)
    end.

parse(Filename, SubDB, Coordinator) ->
    io:format("Parsing: ~p ~n", [Filename]),
    T0 = erlang:now(),
    Coordinator ! {started, {Filename, SubDB}},
    case file:consult(Filename) of
        {ok, FiredProbes} ->
            parse_loop(FiredProbes, Coordinator, Filename, SubDB, T0, 0);
        {error, Reason}   ->
            Coordinator ! {error, Reason}
    end.

parse_loop([], Coordinator, Filename, SubDB, T0, Count) ->
    SubDB ! {insert, {trace_ts, self(), end_of_trace}},
    receive
        {SubDB, done} ->
            T1 = erlang:now(),
            io:format("Parsed ~p entries from ~p in ~p secs.~n",
                      [Count, Filename, ?seconds(T1, T0)]),
            Coordinator ! {self(), done},
            receive
                {ack, Coordinator} ->
                    ok
            end
    end;
parse_loop([FiredProbe|Rest], Coordinator, Filename, SubDB, T0, Count) ->
    TM = firedprobe2msg(FiredProbe),
    M = {insert, TM},
    SubDB ! M,
    parse_loop(Rest, Coordinator, Filename, SubDB, T0, Count + 1).

firedprobe2msg({active, P, MFA, Ts}) when is_binary(P) ->
    {profile, binary_to_term(P), active, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({active, P, Ts}) ->
    {profile, binary_to_term(P), active, 0, dtts2ts(Ts)};
firedprobe2msg({active, S, AS, Ts}) ->
    {profile, scheduler, S, active, AS, dtts2ts(Ts)};
firedprobe2msg({call, P, MFA, Ts}) ->
    {trace_ts, binary_to_term(P), call, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({closed, P, Reason, Ts}) ->
    {trace_ts, binary_to_term(P), Reason, dtts2ts(Ts)};
firedprobe2msg({exit, P, Reason, Ts}) ->
    {trace_ts, binary_to_term(P), exit, Reason, dtts2ts(Ts)};
firedprobe2msg({gc_start, P, Ts}) ->
    {trace_ts, binary_to_term(P), gc_start, [], dtts2ts(Ts)};
firedprobe2msg({gc_end, P, Ts}) ->
    {trace_ts, binary_to_term(P), gc_end, [], dtts2ts(Ts)};
firedprobe2msg({in, P, MFA, S, Ts}) ->
    {trace_ts, binary_to_term(P), in, S, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({in, P, MFA, Ts}) ->
    {trace_ts, binary_to_term(P), in, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({inactive, P, MFA, Ts}) when is_binary(P) ->
    {profile, binary_to_term(P), inactive, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({inactive, P, Ts}) ->
    {profile, binary_to_term(P), inactive, 0, dtts2ts(Ts)};
firedprobe2msg({inactive, S, AS, Ts}) ->
    {profile, scheduler, S, inactive, AS, dtts2ts(Ts)};
firedprobe2msg({open, O, Name, P, Ts}) ->
    {trace_ts, binary_to_term(O), open, binary_to_term(P), Name, dtts2ts(Ts)};
firedprobe2msg({out, P, MFA, S, Ts}) ->
    {trace_ts, binary_to_term(P), out, S, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({out, P, MFA, Ts}) ->
    {trace_ts, binary_to_term(P), out, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg(L={profile_start, _}) ->
    L;
firedprobe2msg(L={profile_stop, _}) ->
    L;
firedprobe2msg({'receive', S, Sz, Ts}) ->
    {trace_ts, binary_to_term(S), 'receive', "", Sz, dtts2ts(Ts)};
firedprobe2msg({register, P, Name, Ts}) ->
    {trace_ts, binary_to_term(P), register, Name, dtts2ts(Ts)};
firedprobe2msg({return_to, P, MFA, Ts}) ->
    {trace_ts, binary_to_term(P), return_to, dtmfa2mfa(MFA), dtts2ts(Ts)};
firedprobe2msg({send, S, R, Sz, Ts}) ->
    {trace_ts, binary_to_term(S), send, "", Sz, binary_to_term(R), dtts2ts(Ts)};
firedprobe2msg({spawn, P, Parent, MFA, Ts}) ->
    {trace_ts, binary_to_term(Parent), spawn, binary_to_term(P), dtmfa2mfa(MFA), dtts2ts(Ts)}.

dtmfa2mfa(DTMFA) when DTMFA == "0" orelse DTMFA == "<exiting>" ->
    0;
dtmfa2mfa(DTMFA) ->
    {M, [$:|DTFA]} = lists:splitwith(fun(C) -> C /= $: end, DTMFA),
    {F, [$/|A]} = lists:splitwith(fun(C) -> C /= $/ end, DTFA),
    {list_to_atom(M), list_to_atom(F), list_to_integer(A)}.

dtts2ts(DTTs) ->
   US = DTTs rem 1000000,
   SS = DTTs div 1000000,
   MS = SS div 1000000,
   S = SS rem 1000000,
   {MS, S, US}.

flush() ->
    receive
        _ -> flush()
    after 0 ->
            ok
    end.

