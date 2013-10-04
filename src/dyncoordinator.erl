-module(dyncoordinator).

-export([start/3, stop/0]).
-export([receiver/1, test/0]). %FIXME: Remove this exports; debug only.

start(_Nodes, _FileSpec, _Options) ->
    ok.

stop() ->
    ok.

receiver(Tracer) ->
  receive
    {Tracer, Msg} -> io:format("Received: ~s~n", [Msg]),
                     receiver(Tracer);
    stop_trace    -> Tracer ! close_port,
                     receiver(Tracer);
    die           -> ok
  end.


%% TODO: Remove! only for testing purposes.
test() ->
  T = spawn(bar@greedy, fun () -> dynprofiler:run() end), %XXX: Hardcoded names!
  C = spawn(fun () -> dyncoordinator:receiver(T) end),
  T ! {C, start_trace}.
