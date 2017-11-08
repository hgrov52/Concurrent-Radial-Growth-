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


% function is executed any time a new election happens
% this is the first function called when the previous leader
% is sent a message for a new election
%
% radial election does most of the work, the purpose of 
% having this function is so that it know when it has completed 
% a full loop of all the nodes
election( Parent, Left, Right, Pid, Name, 
          Priority, Tolerance, NewMaxPriority, N, Deposed, StartTime, Stream ) -> 

  	%previous leader node that starts the radial election
    %checks that it has not been leader before
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

% propogates the election message taking a node with 
% a higher priority as the new leader as long as 
% it is not deposed.
%
% in the case that two nodes have the same priority,
% the >= overrides the previous elected leader 
% so that the higher id is made the leader
%
% the higher id is always left because we recieve the
% nodes in sorted order because of our sort in similation.
%
% the complexity of this function is O(n) because we only 
% complete one round. However, we sort the nodes initially 
% so the complexity of the election is O(radialElection + sort)
% therefore the complexity of this election algorithm is 
% now O(n + nlogn) -> O(nlogn).
radialElection( Parent, Left, Right, Pid, Name, 
          Priority, Tolerance, NewMaxPriority, N, Deposed, StartTime, Leader, Start, Stream ) -> 
	%io:format("Node: ~p, Sending to ~p Case: ~p Leader: ~p Deposed: ~p ~n",[Pid, Left,(self()==Start), Leader, Deposed]),
	case self() == Start of
		true ->
			%back to start, return leader
			Parent ! {leader, Leader, StartTime};
		false ->
			case Deposed of
				true -> 
					Left ! {radialElection, NewMaxPriority, Leader, StartTime, Start, Stream};
				false ->
					case Priority >= NewMaxPriority of
						true ->
							Left ! {radialElection, Priority, self(), StartTime, Start, Stream};
						false ->
							Left ! {radialElection, NewMaxPriority, Leader, StartTime, Start, Stream}
					end
			end
	end,
	agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, NewMaxPriority, N, false, Deposed, StartTime, StartTime ),
    
	ok.

% when an election is going on a leader will send the first message to the left,
% when this happens agents wiill keep updating the time and checking if they need to
% revolt, if they revolt the send a message to the supervisor that says they have revolted, if they revolt they continue to have the same funtionality of passing a message. If a node is a leader, once it recieives a message, it will check with the supervisor if it has been deposed before sending the next message to the left.
% once all required messages are passed, the agent will call itself again
timeIncrement( Parent, Left, Right, Pid, Name, 
               Priority, Tolerance, PriorityList, Time, Leader, N, NodesRevolted, Revolted, Deposed, StartTime ) -> 
    %checks if the current node is the leader
    case self() == (Leader) of
        %node not a leader
        false->
            % able to revolt?
            case (Time - StartTime) >= Tolerance of
                % try to revolt
                true ->
                    % check that it hasent revolted already
                    case Revolted of
                        %if not revolted yet will send supervisor message
                        false ->
                            Parent ! {revolted, Time, Name, Pid },
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, true, Deposed, Time, StartTime );

                        % no need to revolt again keep sending message and updating time
                        true -> 
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, true, Deposed, Time, StartTime )
                    end;
                % no need to revolt
                false ->
                    Left ! {timeIncrement, Time+1, Leader, NodesRevolted},
                    agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, PriorityList, N, Revolted, Deposed, Time, StartTime )

            end;
        % when we complete one round we arrive back to leader and 
        % need to check if the leader has been deposed, so we pause
        % the radial algorithm and ask the supervisor if the leader
        % has been deposed. If so, a new election is called by the 
        % supervisor. if not, radial action continues.         
        true ->
            Parent ! {checkDeposed, Pid, Time},
            receive
                {isDeposed, NewDeposed} ->
                    %check that it has not been deposed
                    case NewDeposed of
                        % will start a new loop
                        false->
                            Left ! {timeIncrement, Time+1, Leader, NodesRevolted },
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, 
                                   PriorityList, N, Revolted, NewDeposed, Time, StartTime );
                        % will start again and wait fot new election to start 
                        true ->
                            agent( Parent, Left, Right, Pid, Name, Priority, Tolerance, 
                                   PriorityList, N, Revolted, NewDeposed, Time, StartTime ),
                            ok

                    end
            end
    end,

    ok.

create(Content) -> spawn(agent,agent,[Content]).