-module(dynprofiler).

-export([run/0]).

%-define(DEBUG, true).

-ifdef(DEBUG).
-define(DBG(S), dbg(S)).
-else.
-define(DBG(_), ok).
-endif.

-type dynutil() :: dtrace | systemtap.

run() ->
  ?DBG("Waiting for 'start_trace'."),
  Coordinator = receive
                  {C, start_trace} -> ?DBG("Starting."), C
                end,
  %% Run the tracer
  TraceUtil = case erlang:system_info(dynamic_trace) of
                none -> erlang:exit({?MODULE, run,
                                     "Dynamic tracing is not available!"});
                T    -> T
              end,
  %%  Script = "./dt-percept2.systemtap", %%XXX: Use this if it doesn't work! :)
  Script = create_script_file(systemtap, [call]),
  Cmd = fmt_command(TraceUtil, os:getpid(), Script),
  ?DBG("Port command: " ++ Cmd),
  Port = open_port({spawn, Cmd}, [{line, 1024}]),
  %% Receive from port
  loop(Port, Coordinator).


loop(Port, Coordinator) ->
  receive
    {Port, {data, {eol, Data}}} ->
      ?DBG(iof("Data: ~s", [Data])),
      Coordinator ! {self(), Data},
      loop(Port, Coordinator);
    close_port ->
      ?DBG("Closing port."),
      true = port_close(Port)
  end.


create_script_file(systemtap, Flags) ->
  Filename = "/tmp/trace-script" ++ unique_id() ++ ".systemtap",
  {ok, ScriptF} = file:open(Filename, [append, raw]),
  %% Create script
  ScriptS = gen_trace_script(systemtap, Flags),
  %% Dump to file
  ok = file:write(ScriptF, ScriptS),
  file:close(ScriptF),
  Filename.

unique_id() ->
  integer_to_list(erlang:phash2({softlab, 42, now()})).

-spec fmt_command(dynutil(), pid(), string()) -> string().
fmt_command(systemtap, TraceePid, Script) ->
  Path = filename:dirname(os:find_executable(get_vm_executable())),
  fl(iof("env PATH=~s/:$PATH ~p -x ~s ~s", [Path, stap, TraceePid, Script])).

get_vm_executable() ->
  case erlang:system_info(smp_support) of
    true  -> "beam.smp";
    false -> "beam"
  end.


%% Functions to generate trace script
-spec gen_trace_script(dynutil(), [atom()]) -> string().
gen_trace_script(Util, Flags) ->
    gen_trace_script(Util, Flags, []).

-spec gen_trace_script(dynutil(), [atom()], [string()]) -> string().
gen_trace_script(_, [], Acc) ->
    fl(lists:reverse(Acc));
gen_trace_script(Util, [Flag|Flags], Acc) ->
    Probes = flag2probes(Flag),
    PPProbes = [mk_probe(Util, Probe) || Probe <- Probes],
    gen_trace_script(Util, Flags,
                     [["// ", atom_to_list(Flag), "\n", PPProbes, "\n"] | Acc]).


-spec mk_probe(dynutil(), string()) -> string().
mk_probe(systemtap, Probe) ->
    EmuSuffix = case erlang:system_info(multi_scheduling) of
                    disabled -> "";
                    _        -> ".smp"
                end,
    fl(["probe process(\"beam", EmuSuffix, "\").mark(\"", Probe, "\") {\n",
        mk_body(systemtap, Probe),
        "}\n"]);
mk_probe(dtrace, Probe) ->
    fl(["erlang*:::", Probe, " {\n",
        mk_body(dtrace, Probe),
        "}\n"]).


-spec mk_body(dynutil(), string()) -> string().
mk_body(systemtap, "message-queued") ->
    "\tprintf(\"{ 'message-queued', \\\"%s\\\", %d }.\\n\", user_string($arg1), $arg2);\n";
mk_body(systemtap, "message-send") ->
    "\tprintf(\"{ 'message-send', \\\"%s\\\", \\\"%s\\\", %d }.\\n\", user_string($arg1), user_string($arg2), $arg3);\n";
mk_body(systemtap, "local-function-entry") ->
    "\tprintf(\"{ 'function-call', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "global-function-entry") ->
    "\tprintf(\"{ 'function-call', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "bif-entry") ->
    "\tprintf(\"{ 'function-call', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "nif-entry") ->
    "\tprintf(\"{ 'function-call', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "function-return") ->
    "\tprintf(\"{ 'function-return', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "bif-return") ->
    "\tprintf(\"{ 'function-return', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "nif-return") ->
    "\tprintf(\"{ 'function-return', \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2));\n";
mk_body(systemtap, "process-spawn") ->
    "\tprintf(\"{ 'process-spawn', \\\"%s\\\", \\\"%s\\\", \\\"%s\\\" }.\\n\", user_string($arg1), user_string($arg2), user_string($arg3));\n".


-spec flag2probes(atom()) -> [string()].
flag2probes('receive') ->
    ["message-queued"];
flag2probes(send) ->
    ["message-send"];
flag2probes(call) ->
    ["local-function-entry", "global-function-entry", "bif-entry", "nif-entry"];
flag2probes(return_to) ->
    ["function-return", "bif-return", "nif-return"];
flag2probes(spawn) ->
    ["process-spawn"];
flag2probes(Flag) ->
    erlang:exit({?MODULE, flag2probe, "Unknown trace/profile flag!", Flag}).


%% Auxiliary functions
fl(L) -> lists:flatten(L).
iof(Fmt, LS) -> io_lib:format(Fmt, LS).
dbg(S) -> io:format("[DEBUG] ~s~n", [S]).
