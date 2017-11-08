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
    case L of
        [{Pid, Address, Name, Priority, Tolerance} | T] ->
        	NodeAddress = list_to_atom(Name++"@localhost"),
        	io:format("NodeAddress: ~p~n",[NodeAddress]),
            [spawn(NodeAddress, agent,agent,[self(), -1, -1, Pid, Name, Priority, Tolerance, 
                                PriorityList, length(PriorityList), false, false, 0, 0])
                                | spawnNodes(T, PriorityList)];
        [] -> 
            []
    end.

initNodes ( ListOfActors) ->

    lists:foreach(fun( N ) ->
    	%io:format("Printing this shit ~p.~n",[ListOfActors]),
    	%io:format("N:  ~p.~n",[N]),
    	
    	SendTo = lists:nth(N +1, ListOfActors),
    	LeftNode = lists:nth( mod((N-1), length(ListOfActors))+1 , ListOfActors),
    	RightNode = lists:nth( mod((N+1), length(ListOfActors))+1 , ListOfActors),
    	SendTo ! { init , LeftNode, RightNode },
        

        % Don't remove
        ok

    end, lists:seq(0, length(ListOfActors)-1)).

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0, _) -> 0.



election(ListOfActors, NodesRevolted, Time, ListOfPriorities, StartTime, Leader, Stream) ->
    % send a message to all agents about starting election
    case ListOfPriorities of
    	[] ->
    		io:format(Stream,"End of simulation~n",[]);
    	_ ->

		    LeaderPriority = lists:max(ListOfPriorities),
		    Leader ! {election, LeaderPriority, Time, Stream},
		    %lists:foreach(fun(N) ->
		    %	SendTo = lists:nth(N, ListOfActors),
		    %	SendTo ! {election, LeaderPriority, Time, Stream}
		    	%io:format("Sent start to ~p.~n",[SendTo])
		    %end, lists:seq(1, length(ListOfActors))),

		    sAgent(ListOfActors, NodesRevolted, Time, -1, ListOfPriorities, StartTime, Stream)
	end.


revolted(ListOfActors, NodesRevolted, Time, Leader ,Name, Agent, ListOfPriorities, StartTime, Stream) ->
    %io:format("ID=~p T=~p~n", [(Name), (Time)]),
    %io:format("member=~p~n", [lists:member(Name, NodesRevolted)]),
    case lists:member(Name, NodesRevolted) of
        true -> 
            Agent ! {ok},
            sAgent(ListOfActors, (NodesRevolted), Time, Leader, ListOfPriorities, StartTime, Stream);

        false ->
            %io:format("name=~p, NodesRe=~p~n", [Name, NodesRevolted]),
            %io:format("Len(NodesRe)=~p len(actors)=~p~n", 
            %          [ (length(NodesRevolted) +1), (length(ListOfActors))]),
            %io:format("name=~p, NodesRe=~p~n", [Name, NodesRevolted]),
                     
            sAgent(ListOfActors, (NodesRevolted ++ [Name]), Time, Leader, ListOfPriorities, StartTime, Stream)
    end,
 
    ok.


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
        % 
        {elected, AgentPid, Time} -> io:format("elected: ~p.~n" ,[AgentPid]);
        %receives a message every time a agent revolts
        {revolted, T, Name, Agent} -> 
        	io:format(Stream,"ID=~p revolted at t=~p~n", [Agent, T]),
            revolted(ListOfActors, NodesRevolted, T, Leader, 
                     Name, Agent, ListOfPriorities, StartTime, Stream);
        % leader will send the supervisor a message asking if it has been deposed
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
    %io:format("Supervisor out of blocking~n"),
    ok.

killNodes(ListOfActors) ->
	lists:foreach(fun(N) ->
			exit(N, "Done with process")
		end ,ListOfActors),
	ok.


%%%%%%%%%%%%%%
%%%MAIN%%%%%%%
%%%%%%%%%%%%%%

run(Filename) ->
	{ok, Stream} = file:open("output.txt" , write),
    % Nodes is a list of input data 
    %io:format("READ FILE~n"),
	Nodes = parser:read(Filename),
    %io:format("Made List of actors~n"),
	ListOfActors = spawnNodes(Nodes, priorityList(Nodes)),
    %io:format("Init the Nodes Left and Right~n"),
    %io:format("~p~n", [ListOfActors]),
    initNodes(ListOfActors),
    %io:format("Sent Election Start~n"),
    List = priorityList(Nodes),

    Start = lists:nth(1,ListOfActors),
    election(ListOfActors, [], 0, List, 0,Start, Stream ),

	file:close(Stream),
	%io:format("Problem here"),
	killNodes(ListOfActors),
	%exit(self(),"End"),
	ok.

%start with process that instanciates each actor
%supervisor calls spawn