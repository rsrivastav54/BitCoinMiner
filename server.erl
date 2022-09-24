%% Authors : Rishabh Srivastav (UF ID : 76599488)
%%           Ashish Sunny Abraham (UF ID : 63887782)   

-module(server).
-export([start/0]).
-export([awaiting_client/3]).
-import(string,[substr/3, right/3, concat/2]).

start() ->  % server execution begins at this function
    {ok, Z_count} = io:read("Enter number of 0s to mine for bitcoin : "),
    {ok, Max_Coins_Count} = io:read("Enter number of coins to mine : "),
    {ok, Miner_count} = io:read("Enter number of workers to spawn : "),
    spawn_many(self(), Z_count, Miner_count),   % call to function to spawn workers as desired by the user
    register(server, self()),   % registering pid of server with the name server
    statistics(runtime),    % statistics helps to calculate CPU time
    {Time, _} = timer:tc(server, awaiting_client, [Z_count,Max_Coins_Count, 0]),    % calculate clock time for CPU utilization
    {_, Time_CPU_Since_Last_Call} = statistics(runtime),
    io:fwrite("Total clock time: ~p\nTotal CPU time ~p\nCPU time/ Run Time ~p\n", [Time/1000, Time_CPU_Since_Last_Call, Time_CPU_Since_Last_Call/(Time/1000)]),
    unregister(server),
    io:fwrite("Ending process... ~n"),
    exit(self(),ok).    % close the existing server process, to begin mining again use server:start()

spawn_many(_, _, 0) ->
    ok;
spawn_many(Pid, Z_count, Miner_count) ->
    spawn(fun() -> start_mining(Pid, Z_count) end),
    spawn_many(Pid, Z_count, Miner_count-1).

start_mining(Pid, Z_count) ->   % start mining
    Name = "rishabhsrivastav;", % name string
    Random_string = base64:encode_to_string(crypto:strong_rand_bytes(8)),   % random string generation
    Hash_string = concat(Name, Random_string),  % concatenate name and random string
    Hash_generated = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Hash_string))]),  % generate hash of concatenated string
    Zero_string = right("", Z_count, $0),   % logic to compare if the hash has equivalent no. of 0s or not
    Max_zero_string = right("", Z_count+1, $0),
    Substring = substr(Hash_generated, 1, Z_count),
    Max_substring = substr(Hash_generated, 1, Z_count+1),
    if
        (Zero_string == Substring) and (Max_zero_string =/= Max_substring)->
            Pid ! {Hash_generated, Hash_string, self()};    % send message from spawned worker to server if bitcoin is found
    true ->
        ok
    end,
    start_mining(Pid, Z_count).

awaiting_client(_,Max_Coins_Count, Max_Coins_Count) ->
    ok; % stop mining when max_coins_count is reached
awaiting_client(Z_count, Max_Coins_Count, Coins_count) ->
    receive % waiting for a miner/worker to connect
        { Client_id }-> 
            Client_id ! { Z_count }, % sending back the number of 0s to check for in hash
            io:fwrite("Received connection from client id : ~p\n", [Client_id]),
            awaiting_client(Z_count, Max_Coins_Count, Coins_count); %% waiting for another miner

        { Coin, Hashstring, Client_id} ->
            io:fwrite(" ~p Bitcoin ~p with string ~p from worker : ~p\n", [Coins_count+1, Coin, Hashstring, Client_id]),
            awaiting_client(Z_count, Max_Coins_Count, Coins_count+1)
    end.