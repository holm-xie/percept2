-module(dt_coordinator).

-export([init/3]).

init(Nodes, FileSpec, Opts) ->

    NodesToProfile = find_nodes_to_profile(Nodes),
    erlang:put(profiled_nodes, NodesToProfile),

    Collectors = spawn_collectors(NodesToProfile, FileSpec),
    erlang:put(collectors, Collectors),

    Profilers = spawn_profilers(NodesToProfile, Collectors, Opts),
    erlang:put(profilers, Profilers),

    loop().

loop() ->
    receive
        {start, Requester}          ->
            Collectors = erlang:get(collectors),
            start_collectors(Collectors),
            Profilers = erlang:get(profilers),
            start_profilers(Profilers),
            ProfiledNodes = erlang:get(profiled_nodes),
            Requester ! {started, ProfiledNodes},
            loop();
        {stop, Requester}           ->
            Profilers = erlang:get(profilers),
            stop_profilers(Profilers),
            wait_for_collectors_and_profilers(Requester);
        {'DOWN', _, _, Pid, Reason} ->
            Collectors = erlang:get(collectors),
            Profilers = erlang:get(profilers),
            case lists:keytake(Pid, 1, Collectors) of 
                {value, {_, Node, _}, NewCollectors} ->
                    case Reason of
                        normal -> ok;
                        _      -> Profiler = lists:keyfind(Node, 2, Profilers),
                                  Profiler ! stop
                    end,
                    erlang:put(collectors, NewCollectors);
                false                                ->   
                    case lists:keytake(Pid, 1, Profilers) of
                        {value, {_, Node, _}, NewProfilers} ->
                            case Reason of
                                normal -> ok;
                                _      -> Collector = lists:keyfind(Node, 2, Collectors),
                                          Collector ! stop
                            end,
                            erlang:put(profilers, NewProfilers);
                        false -> ok
                    end
            end,
            loop()
    end.    

wait_for_collectors_and_profilers(Requester) ->
    Collectors = erlang:get(collectors),
    Profilers = erlang:get(profilers),
    case Collectors == [] andalso Profilers == [] of
        true -> 
            ProfiledNodes = erlang:get(profiled_nodes),
            Requester ! {stopped, ProfiledNodes};
        false ->  
            receive
                {'DOWN', _, _, Pid, Reason} ->
                    case lists:keytake(Pid, 1, Collectors) of
                        {value, {_, Node, _}, NewCollectors} ->
                            erlang:put(collectors, NewCollectors);
                        false                                ->   
                            case lists:keytake(Pid, 1, Profilers) of
                                {value, {_, Node, _}, NewProfilers} ->
                                    erlang:put(profilers, NewProfilers)
                            end    
                    end
            end,
            wait_for_collectors_and_profilers(Requester) 
    end.   

find_nodes_to_profile([]) ->
    [];
find_nodes_to_profile([Node|Nodes]) ->
    case net_adm:ping(Node) of
        pong ->
            case rpc:call(Node, erlang, system_info, [dynamic_trace]) of
                none      -> find_nodes_to_profile(Nodes);
                dtrace    -> find_nodes_to_profile(Nodes);
                systemtap -> [Node | find_nodes_to_profile(Nodes)]
            end;
        pang ->
            find_nodes_to_profile(Nodes)
    end.

spawn_collectors(Nodes, {Prefix, Suffix}) ->
    lists:map(fun(Node) ->
                  Filename = lists:concat([Prefix, '_', Node, '.', Suffix]),
                  {Collector, Monitor} = spawn_monitor(dt_collector, init, [Filename]),
                  {Collector, Node, Monitor}
              end, Nodes).

spawn_profilers(Nodes, Collectors, Opts) ->
    lists:map(fun(Node) ->
                  {Collector, _, _} = lists:keyfind(Node, 2, Collectors),
                  Profiler = erlang:spawn(Node, dt_profiler, init, [Opts, Collector]),
                  Monitor = erlang:monitor(process, Profiler),
                  {Profiler, Node, Monitor}
              end, Nodes).

start_collectors(Collectors) ->
    lists:foreach(fun({Collector, _, _}) -> Collector ! start end, Collectors).

start_profilers(Profilers) ->
    lists:foreach(fun({Profiler, _, _}) -> Profiler ! start end, Profilers).

stop_profilers(Profilers) ->
    lists:foreach(fun({Profiler, _, _}) -> Profiler ! stop end, Profilers).
