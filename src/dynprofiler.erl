-module(dynprofiler).

-export([run/2]).

run(Coordinator, Opts) ->
    DTFramework = erlang:system_info(dynamic_trace),
    % XXX: DTFramework should never be 'none' here (already handled in
    % dyncoordinator.erl).
    {Prbs, Mods, Flags} = opts2prbs(Opts),
    Script  = gen_script(DTFramework, Prbs, Mods, Flags),
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
    io:format("~p~n", [Opts]),
    {Prbs, Mods, Flags} = opts2prbs(Opts, {[], [], []}),
    {lists:usort(Prbs), lists:usort(Mods), lists:usort(Flags)}.
opts2prbs([], {Prbs, Mods, Flags}) ->
    {Prbs, Mods, Flags};
opts2prbs([procs|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive' 
                      | Prbs], Mods, Flags});
opts2prbs([ports|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['port-active', 'port-inactive',
                      'port-open', 'port-exit' 
                      | Prbs], Mods, Flags});
opts2prbs([schedulers|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['scheduler-active', 'scheduler-inactive' 
                      | Prbs], Mods, Flags});
opts2prbs([running|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive', 'process-scheduled', 
                      'process-unscheduled' 
                      | Prbs], Mods, Flags});
opts2prbs([message|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive', 'message-send', 
                      'message-queued' 
                      | Prbs], Mods, Flags});
opts2prbs([migration|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive', 'process-scheduled', 
                      'process-unscheduled' 
                      | Prbs], Mods, [sched | Flags]});
opts2prbs([garbage_collection|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive', 'gc_major-start', 
                      'gc_minor-start', 'gc_major-end', 'gc_minor-end' 
                      | Prbs], Mods, Flags});
opts2prbs([all|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs([procs, ports, schedulers, running, message, migration | Opts],
              {Prbs, Mods, Flags});
opts2prbs([{callgraph, [Ms]}|Opts], {Prbs, Mods, Flags}) ->
    opts2prbs(Opts, {['process-active', 'process-inactive',
                      'process-spawn', 'process-exit',
                      'process-registered', 'process-exclusive_active', 
                      'process-exclusive_inactive', 'process-scheduled', 
                      'process-unscheduled', 'local-function-entry', 
                      'global-function-entry', 'bif-entry', 'nif-entry',
                      'function-return', 'bif-return', 'nif-return' 
                      | Prbs], Mods ++ Ms, Flags});
opts2prbs([_Opt|Opts], {Prbs, Mods}) ->
    opts2prbs(Opts, {Prbs, Mods}).

gen_script(DTFramework, Prbs, Mods, Flags) ->
    fl([mk_intro(DTFramework, Prbs) |
       [mk_probe(DTFramework, Prb, Mods, Flags) || Prb <- Prbs]]).

mk_intro(systemtap, Prbs) ->
    Schedulers = erlang:system_info(schedulers),
    case contains(Prbs, 'scheduler-active') orelse 
         contains(Prbs, 'scheduler-inactive') of
        true  -> fl(["global active_sched_no = 0\n",
                     "global active_scheds[", 
                     integer_to_list(Schedulers), 
                     "]\n"]);
        false -> ""
    end.

mk_probe(systemtap, Prb, Mods, Flags) ->
    fl(["probe process(\"", get_vm_executable(), "\").mark(\"", 
        atom_to_list(Prb), "\") {\n", mk_probe_body(systemtap, Prb, Mods, Flags), 
        "}\n"]).

mk_probe_body(systemtap, 'bif-entry', _Mods, _) ->
    fl(["\tprintf(\"{ 'call', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'bif-return', _Mods, _) ->
    fl(["\tprintf(\"{ 'return_to', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'function-return', _Mods, _) ->
    fl(["\tprintf(\"{ 'return_to', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($ar2), $arg4);\n"]);
mk_probe_body(systemtap, 'gc_major-end', _, _) ->
    fl(["\tprintf(\"{ 'gc_end', %s, %d }\\n\",",
        " user_string($arg1), $arg4);\n"]);
mk_probe_body(systemtap, 'gc_major-start', _, _) ->
    fl(["\tprintf(\"{ 'gc_start', %s, %d }\\n\",",
        " user_string($arg1), $arg4);\n"]);
mk_probe_body(systemtap, 'gc_minor-end', _, _) ->
    fl(["\tprintf(\"{ 'gc_end', %s, %d }\\n\",",
        " user_string($arg1), $arg4);\n"]);
mk_probe_body(systemtap, 'gc_minor-start', _, _) ->
    fl(["\tprintf(\"{ 'gc_start', %s, %d }\\n\",",
        " user_string($arg1), $arg4);\n"]);
mk_probe_body(systemtap, 'global-function-entry', _Mods, _) ->
    fl(["\tprintf(\"{ 'call', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'local-function-entry', _Mods, _) ->
    fl(["\tprintf(\"{ 'call', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'message-queued', _, _) ->
    fl(["\tprintf(\"{ 'receive', %s, %d, %d }\\n\",",
        " user_string($arg1), $arg2, $arg7);\n"]);
mk_probe_body(systemtap, 'message-send', _, _) ->
    fl(["\tprintf(\"{ 'send', %s, %s, %d, %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3, $arg7);\n"]);
mk_probe_body(systemtap, 'nif-entry', _Mods, _) ->
    fl(["\tprintf(\"{ 'call', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'nif-return', _Mods, _) ->
    fl(["\tprintf(\"{ 'return_to', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'port-active', _, _) ->
    fl(["\tprintf(\"{ 'active', %s, %d }\\n\",",
        " user_string($arg1), $arg2);\n"]);
mk_probe_body(systemtap, 'port-exit', _, _) ->
    fl(["\tprintf(\"{ 'closed', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg2), user_string($arg4), $arg5);\n"]);
mk_probe_body(systemtap, 'port-inactive', _, _) ->
    fl(["\tprintf(\"{ 'inactive', %s, %d }\\n\",",
        " user_string($arg1), $arg2);\n"]);
mk_probe_body(systemtap, 'port-open', _, _) ->
    fl(["\tprintf(\"{ 'open', %s, '%s', %s, %d }\\n\",",
        " user_string($arg1), user_string($arg2), user_string($arg3),",
        "  $arg4);\n"]);
mk_probe_body(systemtap, 'process-active', _, _) ->
    fl(["\tprintf(\"{ 'active', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-exclusive_active', _, _) ->
    fl(["\tprintf(\"{ 'active', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-exclusive_inactive', _, _) ->
    fl(["\tprintf(\"{ 'inactive', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-exit', _, _) ->
    fl(["\tprintf(\"{ 'exit', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-inactive', _, _) ->
    fl(["\tprintf(\"{ 'inactive', %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-registered', _, _) ->
    fl(["\tprintf(\"{ 'register', %s, '%s', %d }\\n\",",
        " user_string($arg1), user_string($arg2), $arg3);\n"]);
mk_probe_body(systemtap, 'process-scheduled', _, Flags) ->
    case contains(Flags, sched) of
        true  -> fl(["\tprintf(\"{ 'in', %s, \\\"%s\\\", %d, %d }\\n\",",
                    " user_string($arg1), user_string($arg2), $arg3, $arg4);",
                    "\n"]);
        false -> fl(["\tprintf(\"{ 'in', %s, \\\"%s\\\", %d }\\n\",",
                     " user_string($arg1), user_string($arg2), $arg3);\n"])
    end;
mk_probe_body(systemtap, 'process-spawn', _, _) ->
    fl(["\tprintf(\"{ 'spawn', %s, %s, \\\"%s\\\", %d }\\n\",",
        " user_string($arg1), user_string($arg2), user_string($arg3),",
        " $arg4);\n"]);
mk_probe_body(systemtap, 'process-unscheduled', _, Flags) ->
    case contains(Flags, sched) of
        true  -> fl(["\tprintf(\"{ 'out', %s, \\\"%s\\\", %d, %d }\\n\",",
                    " user_string($arg1), user_string($arg2), $arg3, $arg4);",
                    "\n"]);
        false -> fl(["\tprintf(\"{ 'out', %s, \\\"%s\\\", %d }\\n\",",
                     " user_string($arg1), user_string($arg2), $arg3);\n"])
    end;
mk_probe_body(systemtap, 'scheduler-active', _, _) ->
    fl(["\tif(!active_scheds[$arg1]) {\n",
        "\t\tactive_sched_no++\n",
        "\t\tactive_scheds[$arg1] = 1\n",
        "\t}\n",
        "\tprintf(\"{ 'active', %d, %d, %d }\\n\",",
        " $arg1, active_sched_no, $arg2);\n"]);
mk_probe_body(systemtap, 'scheduler-inactive', _, _) ->
    fl(["\tif(active_scheds[$arg1]) {\n",
        "\t\tactive_sched_no--\n",
        "\t\tdelete active_scheds[$arg1]\n",
        "\t}\n",
        "\tprintf(\"{ 'inactive', %d, %d, %d }\\n\",",
        " $arg1, active_sched_no, $arg2);\n"]).

save_script(S) ->
    SFN = filename:join(["/tmp/", 
                         test_server:temp_name("percept2-trace-script-")]),
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
contains([], _) ->
    false;
contains([H|_], E) when H == E ->
    true;
contains([_|T], E) ->
    contains(T, E).
