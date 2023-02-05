# erlang-project-53607-61726-1
erlang-project-53607-61726-1 created by GitHub Classroom

Ricardo Barqueira, 53607
Luis Almas, 61726

Relatório do Projeto Messaging System de Erlang

In this project we implemented a message system that comprises of a server that receives messages from the clients, the clients that perform sequences of requests to the server
and obtain a response (operation successful or error) and lastly a state manager that receives requests from the server and changes the state of the system based on the request.

To start the program use this command-> main:start(NClients).

We start by saying how many clients will run in our system and some of them will run according to the protocol and some won't:

            start(NClients)->
              Server = server:start_server(),
              create_clients(NClients, Server).

Each client thread will run simultaneously sending synchronous requests to the server (waits for the response before sending another request):

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
 
 The server will create a thread to process each request and sends a request to the state manager:
 
             server_loop(State_Manager, Session_Manager)->
                receive
                    {register, NickName, Client} ->
                        spawn(?MODULE, register_user, [NickName, State_Manager, Client]);
                    {connect, NickName, Client} ->
                        spawn(?MODULE, connect_user, [NickName, State_Manager, Client]);
                    {disconnect, NickName, Client} ->
                        spawn(?MODULE, disconnect_user, [NickName, State_Manager, Client]);
                    {join, Room, NickName, Client} -> 
                        spawn(?MODULE, join, [NickName, Client, Session_Manager, Room]);
                    {leave, Room, NickName, Client} ->
                        spawn(?MODULE, leave, [NickName, Client, Session_Manager, Room]);
                    {write, {NickName, Text}, Room, Client} ->
                        spawn(?MODULE, write_message, [{NickName, Text}, Room, Client, Session_Manager]);
                    state ->
                        spawn(?MODULE, get_state, [State_Manager, Session_Manager])
                end,
                server_loop(State_Manager, Session_Manager).
                
 The state manager will be listening for requests from the server and based on the request an action will be performed that will change the state of the system (if no conflicts are
 detected) and finally will respond to the server if the action was successful or not. The state will include the registered users, connected users and the rooms with their
 respective rooms and messages:
 
               server_state_manager(Users, Connected, Session_Manager) ->
                  receive
                      {add, NickName, Pid} -> 
                          server_state_manager(register_user(NickName, Users, Pid), Connected, Session_Manager);
                      {connect, NickName, Pid} ->
                          server_state_manager(Users, connect_user(NickName, Users, Connected, Pid, Session_Manager), Session_Manager);
                      {disconnect, NickName, Pid} ->
                          server_state_manager(Users, disconnect_user(NickName, Connected, Pid, Session_Manager), Session_Manager);
                      state ->
                          io:format("\nUsers:\n~p\n", [Users]),
                          io:format("\nConected:\n~p\n", [Connected])
                  end,
                  server_state_manager(Users, Connected, Session_Manager).


              server_session_manager(Rooms, Connected) -> 
                  receive
                      {join, NickName, Room, Pid} ->
                          IsConnected = check_nick_exists(NickName, Connected),
                          if
                              IsConnected == true ->
                                  server_session_manager(join_room(Rooms, NickName, Room, Pid), Connected);
                              true ->
                                  Pid ! not_connected,
                                  server_session_manager(Rooms, Connected)
                          end;
                      {check_user_rooms, NickName, Pid} ->
                          check_user_rooms_session_manager(NickName, Pid, Rooms),
                          server_session_manager(Rooms, Connected);
                      {leave, NickName, Room, Pid} ->
                          IsConnected = check_nick_exists(NickName, Connected),
                          if
                              IsConnected == true ->
                                  server_session_manager(leave_room(NickName, Pid, Rooms, Room), Connected);
                              true ->
                                  Pid ! not_connected,
                                  server_session_manager(Rooms, Connected)
                          end;
                      {write_message,{NickName,Text},Room,Pid} ->
                          IsConnected = check_nick_exists(NickName, Connected),
                          if
                              IsConnected == true ->
                                  server_session_manager(write_message({NickName,Text}, Pid, Rooms, Room), Connected);
                              true ->
                                  Pid ! not_connected,
                                  server_session_manager(Rooms, Connected)
                          end;
                      {update_connected, Updated_Connected} ->
                          server_session_manager(Rooms, Updated_Connected);
                      state ->
                          io:format("\nRooms:\n~p\n", [Rooms])
                  end.
                  
Finally the server sends the response to the client which receives the response and sends another request or says that it's done. After all clients are done the final state of
the system is shown.
 
 We can conclude the program satisfies the envisaged properties: All threads run at the same time and don't depend on each other to run (processes don't have to wait
 for each other to send messages) so we can say that we have Deadlock Freedom and Starvation Freedom, because we have a message queue in the state manager
 the order in which the states are updated is sequential and the state manager is the only entity that can change the state of the system so there are no data races,
 because the processes are actors no memory is shared between them so we guarantee that there is Mutual Exclusion.
 

