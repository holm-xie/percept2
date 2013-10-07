-module(dynprofiler).

-export([run/2]).

run(Coordinator, Opts) ->
    DTFramework = erlang:system_info(dynamic_trace),
    %% XXX: DTFramework should never be 'none' here (already handled in
    %% dyncoordinator.erl).
    {Prbs, Mods} = opts2prbs(Opts),
    Script  = gen_script(DTFramework, Prbs, Mods),
    ScriptF = save_script(Script),
    loop(DTFramework, ScriptF, Coordinator).

loop(DTFramework, ScriptF, Coordinator) ->
    receive
        start ->
            Cmd = fmt_command(DTFramework, ScriptF),
            P = open_port({spawn, Cmd}, [{line, 1024}]),
            put(port, P),
            loop(DTFramework, ScriptF, Coordinator);
        stop  ->
            P = get(port),
            true = port_close(P),
            ok = file:delete(ScriptF);
        {_, {data, {eol, Data}}} ->
            Coordinator ! {self(), Data},
            loop(DTFramework, ScriptF, Coordinator)
    end.

opts2prbs(Opts) ->
    {Prbs, Mods} = opts2prbs(Opts, {[], []}),
    {lists:usort(Prbs), lists:usort(Mods)}.
opts2prbs([], {Prbs, Mods}) ->
    {Prbs, Mods};
opts2prbs([procs|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive'
                      | Prbs], Mods});
opts2prbs([ports|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['port-active', 'port-inactive',
                      'port-open', 'port-exit',
                      'port-registered', 'port-unregistered' | Prbs], Mods});
opts2prbs([schedulers|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['scheduler-active', 'scheduler-inactive' | Prbs], Mods});
opts2prbs([running|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive',
                      'process-scheduled', 'process-unscheduled',
                      'process-scheduled_exiting', 'process-unscheduled_exiting',
                      'process-unscheduled_exited' | Prbs], Mods});
opts2prbs([message|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive',
                      'message-send', 'message-queued' | Prbs], Mods});
opts2prbs([migration|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive',
                      'process-scheduled', 'process-unscheduled',
                      'process-migrate' | Prbs], Mods});
opts2prbs([garbage_collection|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive',
                      'gc_major-start', 'gc_minor-start',
                      'gc_major-end', 'gc_minor-end' | Prbs], Mods});
opts2prbs([all|Opts], {Prbs, Mods}) ->
    opts2prbs([procs, ports, schedulers, running, message, migration | Opts],
              {Prbs, Mods});
opts2prbs([{callgraph, [Ms]}|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-unregistered',
                      'process-link', 'process-unlink',
                      'process-getting_linked', 'process-getting_unlinked',
                      'process-exclusive_active', 'process-exclusive_inactive',
                      'process-scheduled', 'process-unscheduled',
                      'local-function-entry', 'global-function-entry',
                      'bif-entry', 'nif-entry',
                      'function-return', 'bif-return', 'nif-return' | Prbs],
                    Mods ++ Ms});
opts2prbs([_Opt|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {Prbs, Mods}).

gen_script(DTFramework, Prbs, Mods) ->
    fl([mk_probe(DTFramework, Prb, Mods) || Prb <- Prbs]).

mk_probe(systemtap, Prb, Mods) ->
    fl(["probe process(\"", get_vm_executable(), "\").mark(\"", Prb, "\") {\n", 
        mk_body(systemtap, Prb, Mods), "}\n"]).

mk_body(systemtap, 'bif-entry', _Mods) ->
    fl(["\tprintf(\"{ 'call', \\\"%s\\\", \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_body(systemtap, 'bif-return', _Mods) ->
    "";
mk_body(systemtap, 'function-return', _Mods) ->
    "";
mk_body(systemtap, 'gc_major-end', _) ->
    "";
mk_body(systemtap, 'gc_major-start', _) ->
    "";
mk_body(systemtap, 'gc_minor-end', _) ->
    "";
mk_body(systemtap, 'gc_minor-start', _) ->
    "";
mk_body(systemtap, 'global-function-entry', _Mods) ->
    fl(["\tprintf(\"{ 'call', \\\"%s\\\", \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_body(systemtap, 'local-function-entry', _Mods) ->
    fl(["\tprintf(\"{ 'call', \\\"%s\\\", \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_body(systemtap, 'message-queued', _) ->
    "";
mk_body(systemtap, 'message-send', _) ->
    "";
mk_body(systemtap, 'nif-entry', _Mods) ->
    "";
mk_body(systemtap, 'nif-return', _Mods) ->
    "";
mk_body(systemtap, 'port-active', _) ->
    "";
mk_body(systemtap, 'port-exit', _) ->
    "";
mk_body(systemtap, 'port-inactive', _) ->
    "";
mk_body(systemtap, 'port-open', _) ->
    "";
mk_body(systemtap, 'port-registered', _) ->
    "";
mk_body(systemtap, 'port-unregistered', _) ->
    "";
mk_body(systemtap, 'process-active', _) ->
    "";
mk_body(systemtap, 'process-exclusive_active', _) ->
    "";
mk_body(systemtap, 'process-exclusive_inactive', _) ->
    "";
mk_body(systemtap, 'process-exit', _) ->
    "";
mk_body(systemtap, 'process-getting_linked', _) ->
    "";
mk_body(systemtap, 'process-getting_unlinked', _) ->
    "";
mk_body(systemtap, 'process-inactive', _) ->
    "";
mk_body(systemtap, 'process-link', _) ->
    "";
mk_body(systemtap, 'process-migrate', _) ->
    "";
mk_body(systemtap, 'process-registered', _) ->
    "";
mk_body(systemtap, 'process-scheduled', _) ->
    "";
mk_body(systemtap, 'process-scheduled_exiting', _) ->
    "";
mk_body(systemtap, 'process-spawn', _) ->
    "";
mk_body(systemtap, 'process-unlink', _) ->
    "";
mk_body(systemtap, 'process-unregistered', _) ->
    "";
mk_body(systemtap, 'process-unscheduled', _) ->
    "";
mk_body(systemtap, 'process-unscheduled_exited', _) ->
    "";
mk_body(systemtap, 'process-unscheduled_exiting', _) ->
    "";
mk_body(systemtap, 'scheduler-active', _) ->
    "";
mk_body(systemtap, 'scheduler-inactive', _) ->
    "".

save_script(S) ->
    SFN =
      filename:join(["/tmp/", test_server:make_temp("percept2-trace-script-")]),
    {ok, SF} = file:open(SFN, [write, raw]),
    ok = file:write(SF, S),
    file:close(SF),
    SFN.

fmt_command(systemtap, SF) ->
    Exec = filename:dirname(os:find_executable(get_vm_executable())),
    fl(["env PATH=", Exec, "/:$PATH ", "stap -x ", os:getpid(), " ", SF]).

get_vm_executable() ->
    case erlang:system_info(multi_scheduling) of
        disabled -> "beam";
        _        -> "beam.smp"
    end.

%% Auxiliary functions
fl(L) -> 
    lists:flatten(L).
