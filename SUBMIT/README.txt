Henry Grover   | groveh
Carlos Calero  | calerc

%%%%%%%%%%%%%%%%
%%% Features %%%
%%%%%%%%%%%%%%%%

We implement a simple radial growth algorithm according the the programming assignment 2 instruction page. 

Our election depends on the nodes being sorted, therefore we implement a simple quick sort algorithm to sord the nodes based on Pid. The complexity of our election algorithm is O(nlogn) because it is dependent on our radialElection function in agent.erl as well as our sort function in simulation.erl, so the overall complexity is O(radialElection) + O(sort). radialElection is O(n) because it comoletes one circle around touching each node exactly once. sort is nlogn because it implements a quick sort of that complexity. Therefore the complexity is O(n + nlogn) which therefore is O(nlogn) time. 

We use timeIncrement as our propogate function to circulate around the circle to the left as specified in the instructions. After each complete circle that arrives back at the leader, we check if that leader has been deposed. If this leader has been deposed, the supervisor sends a new election message to this now deposed leader in order to find a new leader, and the propogation is terminated. If the leader is not deposed, propogation simply continues around the circle again. 

Revolting occurs during propogation, and any node that revolts is added to a list. The number of revolted nodes is checked by the supervisor when the leader asks if they have been deposed. 

All necessary information is printed to a stream to an output file. 

The only info printed to the console is an ok message signalling the termination of the program. 

%%%%%%%%%%%%%%%%
%%%   Bugs   %%%
%%%%%%%%%%%%%%%%

In our distributed solution, occasionally the creation of nodes occurs too slowly and the program moves on to use them before they are ready. This doesnt usually happen but there is a small chance that it could happen in the first few executions of the distrubuted solution. This problem is not caused by nodes not being cleaned up from the previous program execution, because most of the time the distributed solution can be run over and over without issue. A potential solution could be pausing the supervisor process until the nodes are ready for use. 

