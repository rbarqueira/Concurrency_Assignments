-module(client).
-export([start_good_client/3, start_bad_client/3, client_loop/4, client_loop_prep/4, start_end_clients_listenner/2, end_clients_listenner_loop/2]).

start_end_clients_listenner(NClients, ServerPid) ->
    spawn(?MODULE, end_clients_listenner_loop, [NClients, ServerPid]).

start_good_client(ServerPid, Index, EndListenner)->
    spawn(?MODULE, client_loop_prep, [ServerPid, true, Index, EndListenner]).

start_bad_client(ServerPid, Index, EndListenner) ->
    spawn(?MODULE, client_loop_prep, [ServerPid, false, Index, EndListenner]).

client_loop_prep(ServerPid, IsGood, Index, EndListenner) ->
    if IsGood == true ->
        ClientName =  "Good" ++ integer_to_list(Index),
        client_loop(ClientName, ServerPid, good_client_messages(ClientName, "#Room", self()), EndListenner);
    true ->
        ClientName =  "Bad" ++ integer_to_list(Index),
        client_loop(ClientName, ServerPid, bad_client_messages(ClientName, "Room", self()), EndListenner)
    end.
client_loop(ClientName, _, [], EndListenner) -> io:format("~s is done\n", [ClientName]), EndListenner ! iamdone;
client_loop(ClientName, ServerPid, [H|T], EndListenner) ->
    ServerPid ! H,
    io:format("~s sent: ~p\n",[ClientName, H]),
    receive
        ok -> io:format("~s received: ok\n", [ClientName]);
        already_registered -> io:format("~s received: already_registered\n", [ClientName]);
        unknown_user -> io:format("~s received: unknown_user", [ClientName]);
        still_in_chat_room -> io:format("~s received: still_in_chat_room \n", [ClientName]);
        not_connected -> io:format("~s received: not_connected \n", [ClientName]);
        already_joined -> io:format("~s received: already_joined\n", [ClientName]);
        not_joined -> io:format("~s not_joined \n", [ClientName]);
        invalid_room_name -> io:format("~s invalid_room_name\n", [ClientName])
    end,
    client_loop(ClientName, ServerPid, T, EndListenner).

end_clients_listenner_loop(NClients, ServerPid)->
    if NClients == 0->
        ServerPid ! state;
    true->
        receive
            iamdone -> end_clients_listenner_loop(NClients-1, ServerPid)
        end
    end.

good_client_messages(Name, Room, Pid) ->
    [
        {register, Name, Pid},
        {join, Room, Name, Pid},
        {write, {Name, "Radom message"}, Room, Pid},
        {leave, Room, Name, Pid},
        {disconnect, Name, Pid}
        ].

bad_client_messages(Name, Room, Pid) ->
    [
        {join, Room, Name, Pid},
        {write, {Name, "Radom message"}, Room, Pid},
        {leave, Room, Name, Pid},
        {disconnect, Name,Pid},
        {register, Name, Pid}
        ].