-module(agent).
-export([create/1,agent/13]).


agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime) ->
	receive

        % this is the Init meassage that tells the agent who is to the left and right
		{init, NewLeft, NewRight } ->
			agent(Parent, NewLeft, NewRight, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime);

	    % this message tells the agent an election has started	
        {election, NewMaxPriority, NewTime, Stream} -> 
            election( Parent, Left, Right, Pid, Name, 
                      Priority, Tolerance, NewMaxPriority, N, Deposed, NewTime, Stream );
        {radialElection, NewMaxPriority, Leader, NewTime, Start, Stream} ->
        	radialElection( Parent, Left, Right, Pid, Name, 
                      Priority, Tolerance, NewMaxPriority, N, Deposed, NewTime, Leader, Start, Stream );
       
        % will receive a message from the left node 
        % with the time the node received a message.
        {timeIncrement, NewTime, Leader, []} ->
            timeIncrement( Parent, Left, Right, 
                           Pid, Name, Priority, 
                           Tolerance, PriorityList, NewTime, Leader, N, [], Revolted, Deposed, StartTime );
	 	{getPid} -> 
	 		Parent ! {returnPid, Pid},
            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime ),
    		ok
    end,

    agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime ).

election( Parent, Left, Right, Pid, Name, 
          Priority, Tolerance, NewMaxPriority, N, Deposed, StartTime, Stream ) -> 

  	%previous leader node that starts the radial election
	%io:format("Node: ~p, Sending to ~p~n",[Pid, Left]),

	case Deposed of
       	false -> 
       		%if its not deposed, it is now the leader and should send its priority as max priority
            Left ! {radialElection, Priority, self(), StartTime, self(), Stream};
        true -> 
        	%this node is deposed, and should send itself as leader but a max priority of 0
        	Left ! {radialElection, 0, self(), StartTime, self(), Stream}
    end,
    agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, NewMaxPriority, N, false, Deposed, StartTime, StartTime ),
    
    ok.

radialElection( Parent, Left, Right, Pid, Name, 
          Priority, Tolerance, NewMaxPriority, N, Deposed, StartTime, Leader, Start, Stream ) -> 
	%io:format("Node: ~p, Sending to ~p Case: ~p Leader: ~p Deposed: ~p ~n",[Pid, Left,(self()==Start), Leader, Deposed]),
	case self() == Start of
		true ->
			%back to start, return leader
			%io:format(Stream,"ID=~p became leader at t=~p~n", [Leader, StartTime]),
			Parent ! {leader, Leader, StartTime};
			%Left ! {timeIncrement, StartTime+1, self(), []};
		false ->
			case Deposed of
				true -> 
					Left ! {radialElection, NewMaxPriority, Leader, StartTime, Start, Stream};
				false ->
					case Priority > NewMaxPriority of
						true ->
							Left ! {radialElection, Priority, self(), StartTime, Start, Stream};
						false ->
							Left ! {radialElection, NewMaxPriority, Leader, StartTime, Start, Stream}
					end
			end
	end,
	agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, NewMaxPriority, N, false, Deposed, StartTime, StartTime ),
    
	ok.

timeIncrement( Parent, Left, Right, Pid, Name, 
               Priority, Tolerance, PriorityList, Time, Leader, N, NodesRevolted, Revolted, Deposed, StartTime ) -> 
    %checks of you are the leader
    %io:format("ID~p got a message from left T=~p Tolerance=~p T-ST= ~p Priority: ~p Max: ~p ~n" , [Pid, Time, Tolerance, (Time - StartTime), Priority,PriorityList]),
    %io:format("Leader~p~n",  [(Priority == lists:max(PriorityList))]),
    case self() == (Leader) of
        %node not a leader
        false->
            % able to revolt?

            case (Time - StartTime) >= Tolerance of
                true ->
                    %need to revolt
                    %io:format("Leader~p~n",  [Revolted]),
                    case Revolted of
                        %if not revolted yet
                        false ->
                            %io:format("ID=~p revolted at t=~p~n", [Pid, Time]),
                            Parent ! {revolted, Time, Name, Pid },
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, true, Deposed, Time, StartTime );

                        % no need to revolt again
                        true -> 
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, true, Deposed, Time, StartTime )
                    end;
                % no need to revolt
                false ->
                    Left ! {timeIncrement, Time+1, Leader, NodesRevolted},
                    agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime )

            end;
        %node is a leader need to check that it is not deposed
        true ->
            %io:format("Leader Check: ~p at t=~p~n",[Name,Time]),
            Parent ! {checkDeposed, Pid, Time},
            %io:format("t=~p~n", [Time]),
            receive
                {isDeposed, NewDeposed} ->
                    %check that it has not been deposed
                    case NewDeposed of
                        false->
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, 
                                   PriorityList, N, Revolted, NewDeposed, Time, StartTime );
                        true ->
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, 
                                   PriorityList, N, Revolted, NewDeposed, Time, StartTime ),
                            ok

                    end
            end
    end,

    ok.

create(Content) -> spawn(agent,agent,[Content]).