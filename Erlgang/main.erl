-module(main).
-import(server, [start_server/0]).
-import(client, [start_good_client/2, start_bad_client/2]).
-export([start/1]).

start(NClients)->
    Server = server:start_server(),
    ClientsListenner = client:start_end_clients_listenner(NClients, Server),
    create_clients(NClients, Server, ClientsListenner).

create_clients(NClients, Server, ClientsListenner) ->
    create_good_client(NClients, Server, ClientsListenner).

create_good_client(NClients, Server, ClientsListenner)->
    if NClients > 0 ->
        client:start_good_client(Server, NClients, ClientsListenner),
        create_bad_client(NClients-1, Server, ClientsListenner);
    true ->
        clients_created
    end.

create_bad_client(NClients, Server, ClientsListenner)->
    if NClients > 0 ->
        client:start_bad_client(Server, NClients, ClientsListenner),
        create_good_client(NClients-1, Server, ClientsListenner);
    true ->
        clients_created
    end.
