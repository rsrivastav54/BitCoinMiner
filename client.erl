%% Authors : Rishabh Srivastav (UF ID : 76599488)
%%           Ashish Sunny Abraham (UF ID : 63887782)   

-module(client).
-import(string,[substr/3, right/3, concat/2]).
-export([start/1]).

start_mining(Z_count, Server_address) -> % start mining
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
            {server, Server_address} ! {Hash_generated, Hash_string, self()};   % send message from spawned worker at client node
                                                                                % to server if bitcoin is found
    true ->
        ok
    end,
    start_mining(Z_count, Server_address).

connect_to_master(Ipaddress) -> % function for establishing client connection with server
    Server_address = list_to_atom(concat("server", concat("@", Ipaddress))),
    {server, Server_address} ! {self()},    % sending client's pid to server
    receive
        { Z_count } -> start_mining(Z_count, Server_address)    % receiving number of leading 0s to check for bitcoin from server
    end.


start(Ipaddress) -> % function to spawn client worker
    spawn(fun() -> connect_to_master(Ipaddress) end).