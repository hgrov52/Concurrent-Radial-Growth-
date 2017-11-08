-module(simulation).
-export([run/1]).


%%%%%%%%%%%%%%%
%%%HELPERS%%%%%%
%%%%%%%%%%%%%%%


% Make a list of the agents priorities
priorityList(Data) ->
    case Data of
        [{_, _, _, Priority, _} | T] ->
            [  Priority  | priorityList(T)];
        [] -> 
            []
    end.

% makes a list of actor pids
spawnNodes(L, PriorityList ) ->
    % if list > 0 will spanwn the new agent and d add the PID to the list
    case L of
        % uses pattern matching
        [{Pid, Address, Name, Priority, Tolerance} | T] ->
            NodeAddress = list_to_atom(Name++"@localhost"),
            [spawn(NodeAddress, agent,agent,[self(), -1, -1, Pid, Name, Priority, Tolerance, 
                                PriorityList, length(PriorityList), false, false, 0, 0])
                                | spawnNodes(T, PriorityList)];
        [] -> 
            []
    end.

initNodes ( ListOfActors) ->

    lists:foreach(fun( N ) ->  
        SendTo = lists:nth(N +1, ListOfActors),
        LeftNode = lists:nth( mod((N-1), length(ListOfActors))+1 , ListOfActors),
        RightNode = lists:nth( mod((N+1), length(ListOfActors))+1 , ListOfActors),
        SendTo ! { init , LeftNode, RightNode },
        ok

    end, lists:seq(0, length(ListOfActors)-1)).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.


% will start election or end the program based on if there agents that can still be elected
election(ListOfActors, NodesRevolted, Time, ListOfPriorities, StartTime, Leader, Stream) ->
    case ListOfPriorities of
        % when there are no more agents that can be leader
        [] ->
            io:format(Stream,"End of simulation~n",[]);

        _ ->
            % at least one agent can still be a leader, so send an election message to the previous leader
            LeaderPriority = lists:max(ListOfPriorities),
            Leader ! {election, LeaderPriority, Time, Stream},
            sAgent(ListOfActors, NodesRevolted, Time, -1, ListOfPriorities, StartTime, Stream)
    end.

% will execute when the sAgent receives a message from an agent that want to revolt 
revolted(ListOfActors, NodesRevolted, Time, Leader ,Name, Agent, ListOfPriorities, StartTime, Stream) ->
    case lists:member(Name, NodesRevolted) of
        % if the agent already revolted
        true -> 
            Agent ! {ok},
            sAgent(ListOfActors, (NodesRevolted), Time, Leader, ListOfPriorities, StartTime, Stream);
        
        % if the agent that sent the message has not already revolted 
        false ->
            % updates list of nodes revolted
            sAgent(ListOfActors, (NodesRevolted ++ [Name]), Time, Leader, ListOfPriorities, StartTime, Stream)
    end,
 
    ok.

% supervisor agent, continually called in order to recieve the messages
sAgent(ListOfActors, NodesRevolted, Time, Leader , ListOfPriorities, StartTime, Stream) ->
    receive
        % message stop the sAgent as it dosent call itself again
        {stop} -> io:format("stop~n");
        % receives a message with the new leader
        {leader, NewLeader, Time} -> 
            NewLeader ! {getPid},
            receive
                {returnPid,NewLeaderPid} -> 
                    io:format(Stream,"ID=~p became leader at t=~p~n", [NewLeaderPid, Time])
            end,
            NewLeader ! {timeIncrement, Time, NewLeader,[]},
            sAgent(ListOfActors, NodesRevolted, Time, NewLeader, ListOfPriorities, StartTime, Stream);
        {elected, AgentPid, Time} -> io:format("elected: ~p.~n" ,[AgentPid]);
        %receives a message every time a agent revolts
        {revolted, T, Name, Agent} -> 
            io:format(Stream,"ID=~p revolted at t=~p~n", [Agent, T]),
            revolted(ListOfActors, NodesRevolted, T, Leader, 
                     Name, Agent, ListOfPriorities, StartTime, Stream);
		% leader will send the supervisor a message asking if it has been deposed
        % if half the nodes have revolted it will send backa message to the leader 
        % updating the status        
        {checkDeposed, ID, NewTime} ->
            case (length(NodesRevolted) + 1) > (length(ListOfActors)+1)/2 of
                 true ->
                    io:format(Stream,"ID=~p was deposed at t=~p~n", [ID, NewTime]),
                    Leader ! {isDeposed, true},
                    NewList = lists:delete(lists:max(ListOfPriorities), ListOfPriorities),
                    election(ListOfActors, [], NewTime+1, NewList, NewTime+1, Leader, Stream);
                 false ->  
                    Leader ! {isDeposed, false},
                    sAgent(ListOfActors, NodesRevolted, NewTime, Leader, ListOfPriorities, StartTime, Stream)
            end;
        {ok} -> ok

    end,
    ok.

% used to clean up the nodes for the distributed solution
killNodes(ListOfActors) ->
    lists:foreach(fun(N) ->
            exit(N, "Done with process")
        end ,ListOfActors),
    ok.


%simple quick sort to allow for easy election                                                            
sort([Pivot|T]) ->                                                              
    sort([ X || X <- T, lessThan(X, Pivot)]) ++ [Pivot] ++ sort([ X || X <- T, greaterThan(X, Pivot)]);                    

sort([]) -> [].                                                                 
                                                                            
lessThan({Pid, _, _, _, _}, {Pivot, _, _, _, _}) ->                             
    Pid < Pivot.                                                                

greaterThan({Pid, _, _, _, _}, {Pivot, _, _, _, _}) ->                          
    Pid > Pivot.


%%%%%%%%%%%%%%
%%%MAIN%%%%%%%
%%%%%%%%%%%%%%

run(Filename) ->
    {ok, Stream} = file:open("output.txt" , write),
    % Nodes is a list of input data 
    Nodes = sort(parser:read(Filename)),
    ListOfActors = spawnNodes(Nodes, priorityList(Nodes)),
    initNodes(ListOfActors),
    List = priorityList(Nodes),
    Start = lists:nth(1,ListOfActors),
    % starts a new election
    election(ListOfActors, [], 0, List, 0,Start, Stream ),
    file:close(Stream),
    killNodes(ListOfActors),
    %exit(self(),"End"),
    ok.