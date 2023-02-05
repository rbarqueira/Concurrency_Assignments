-module(server).
-import(server_state, [start_session_manager/0, start_server_state_manager/1]).
-export([start_server/0, server_loop/2, register_user/3, connect_user/3, disconnect_user/3, join/4, leave/4, write_message/4, get_state/2]).

start_server() ->
    io:format("Server running\n"),
    Session_Manager = server_state:start_session_manager(),
    State_Manager = server_state:start_server_state_manager(Session_Manager), 
    spawn(?MODULE, server_loop, [State_Manager, Session_Manager]).

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

get_state(State_Manager, Session_Manager) ->
    io:format("\nServer state:\n"),
    State_Manager ! state,
    Session_Manager ! state.

disconnect_user(Nickname, State_Manager, Client)->
    State_Manager ! {disconnect, Nickname, self()},
    receive
        ok ->
            Client ! ok;
        still_in_chatroom ->
            Client ! still_in_chatroom;
        not_connected ->
            Client ! not_connected
    end.

connect_user(Nickname, State_Manager, Client)->
    State_Manager ! {connect, Nickname, self()},
    receive
            already_connected ->
                Client ! already_connected;
            unknown_user ->
                Client ! unknown_user;
            ok ->
                Client ! ok
        end.

register_user(Nickname, State_Manager, Client) ->
    State_Manager ! {add, Nickname, self()},
    receive
        true ->
            Client ! ok,
            State_Manager ! {connect, Nickname, self()};
        false ->
            Client ! already_registered
    end.
join(NickName, Client, Session_Manager, Room) ->
    Room_Is_Valid = validate_room_name(Room),

    if
        Room_Is_Valid == ok ->
            Session_Manager ! {join, NickName, Room, self()},
            receive
                ok ->
                    Client ! ok;
                already_joined ->
                    Client ! already_joined;
                not_connected ->
                    Client ! not_connected
            end;
        true ->
            Client ! invalid_room_name
    end.

leave(NickName, Client, Session_Manager, Room) ->
    Session_Manager ! {leave, NickName, Room, self()},
    receive
        ok->
            Client ! ok;
        not_joined->
            Client ! not_joined;
        not_connected ->
            Client ! not_connected;
        invalid_room ->
            Client ! invalid_room
    end.

write_message(Message, Room, Client, Session_Manager) ->
    Session_Manager ! {write_message, Message, Room, self()},
    receive
        ok->
            Client ! ok;
        not_joined->
            Client ! not_joined;
        not_connected ->
            Client ! not_connected;
        invalid_room -> 
            Client ! invalid_room
        end.

validate_room_name(Room) ->
    Room_First_Char = lists:sublist(Room, 1, 1),
    if
        Room_First_Char == "#"->
            ok;
        true ->
            invalid_room_name
    end.