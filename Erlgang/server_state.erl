-module(server_state).
-import(lists,[append/2]).

-export[start_session_manager/0, start_server_state_manager/1].
-export[server_state_manager/3, server_session_manager/2].

start_session_manager()->
    Session_Manager = spawn(?MODULE, server_session_manager, [[], []]),
    Session_Manager.

start_server_state_manager(Session_Manager) ->
    State_Manager = spawn(?MODULE, server_state_manager, [[], [], Session_Manager]),
    State_Manager.

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

check_user_rooms_session_manager(_, Pid, [])-> Pid ! ok; 
check_user_rooms_session_manager(NickName, Pid, [H|T])-> 
    Users_In_Channel = element(2, H),
    Check = check_nick_exists(NickName, Users_In_Channel),
    if 
        Check == true ->
            Pid ! still_in_chatroom;
        true->
             check_user_rooms_session_manager(NickName, Pid, T)
    end.


join_room(Rooms, NickName, Room, Pid) ->
    Tuple_room = get_room(Rooms, Room),
    case Tuple_room of
        none ->
            RoomsList = lists:append(Rooms, [{Room, [NickName], []}]),
            Pid ! ok,
            RoomsList;
        _ ->
                CheckUserInRoom = check_user_in_room(NickName, Tuple_room),
                if
                    CheckUserInRoom == true ->
                    Pid ! already_joined,
                    Rooms;
                true ->
                    Pid ! ok,
                    Updated_Rooms = add_user_to_room(Rooms, NickName, Room, Tuple_room),
                    Updated_Rooms
                 end
    end.
    
leave_room(NickName, Pid, Rooms, Room) ->
    Selected_Room = get_room(Rooms, Room),
    if Selected_Room == none ->
        Pid ! invalid_room,
        Rooms;
    true ->
        Check_User = check_user_in_room(NickName, Selected_Room),
        if
            Check_User == true ->
                Updated_Rooms = remove_user_from_room(NickName, Rooms, Room, Selected_Room),
                Pid ! ok,
                Updated_Rooms;
            true ->
                Pid ! not_joined,
                Rooms
        end
    end.

write_message(Message, Pid, Rooms, Room) ->
    Selected_Room = get_room(Rooms,Room),
    if Selected_Room == none ->
        Pid ! invalid_room,
            Rooms;
        true ->
            Check_User = check_user_in_room(element(1,Message), Selected_Room),
            if
                Check_User == true ->
                    Updated_Rooms = write_message_in_room(Message,Rooms,Room,Selected_Room),
                    Pid ! ok,
                    Updated_Rooms;
                true ->
                    Pid ! not_joined,
                    Rooms
            end
    end.

write_message_in_room(Message, Rooms, Room, Tuple_room) ->
    Messages = element(3, Tuple_room),
    Appended_Messages = lists:append(Messages, [Message]),
    Updated_Room = {Room, element(2,Tuple_room), Appended_Messages},
    update_room(Rooms, Updated_Room, Room).

remove_user_from_room(NickName, Rooms, Room, Tuple_room) ->
    Users = element(2, Tuple_room),
    Deleted_User_List = lists:delete(NickName, Users),
    Updated_Room = {Room, Deleted_User_List, element(3, Tuple_room)},
    update_room(Rooms, Updated_Room, Room).

add_user_to_room(Rooms, NickName, Room, Tuple_room) ->
    Users = element(2, Tuple_room),
    Appended_Users = lists:append(Users, [NickName]),
    Updated_Room = {Room, Appended_Users, element(3, Tuple_room)},
    update_room(Rooms, Updated_Room, Room).

update_room(Rooms, Updated_Room, Room) ->
    Updated_Rooms = update_rooms_list(Rooms, Updated_Room, Room, []),
    Updated_Rooms.

update_rooms_list([], _, _, Updated_Rooms) -> Updated_Rooms;
update_rooms_list([H|T], Updated_Room, Room, Updated_Rooms) ->
    case H of
        {Room, _, _} -> 
            Appended_Rooms = lists:append(Updated_Rooms, [Updated_Room]),
            update_rooms_list(T, Updated_Room, Room, Appended_Rooms);
        _ -> 
            Appended_Rooms = lists:append(Updated_Rooms, [H]),
            update_rooms_list(T, Updated_Room, Room, Appended_Rooms)
    end.

get_room([], _) -> none;
get_room([H|T], Room) ->
    case H of
        {Room, _, _} -> H;
        _ -> get_room(T, Room)
    end.

disconnect_user(NickName, Connected, Pid, Session_Manager) ->
    ExistsConnected = check_nick_exists(NickName, Connected),
    if
        ExistsConnected == true ->

            Check_User_Rooms = check_user_rooms_state_manager(NickName, Session_Manager),  

            if
                Check_User_Rooms == ok ->
                    ConnectedList = lists:delete(NickName, Connected),
                    Pid ! ok,
                    send_updated_connected_users(ConnectedList, Session_Manager),
                    ConnectedList;
                true ->
                    Pid ! still_in_chat_room,
                    Connected
                end;
        true ->
            Pid ! not_connected,
            Connected
    end.

send_updated_connected_users(Connected, Pid)->
    Pid ! {update_connected, Connected}.

check_user_rooms_state_manager(NickName, Session_Manager)->
    Session_Manager ! {check_user_rooms, NickName, self()},
    receive
        ok ->   
            ok;
        still_in_chat_room ->
            still_in_chat_room
    end.
    
connect_user(NickName, Users, Connected, Pid, Session_Manager)->
    ExistsUsers = check_nick_exists(NickName, Users),
    if
        ExistsUsers == true ->
            ExistsConnected = check_nick_exists(NickName, Connected),
            if
                ExistsConnected == true ->
                    Pid ! already_connected,
                    Connected;
                true ->
                    Pid ! ok,
                    ConnectedList = lists:append(Connected, [NickName]),
                    send_updated_connected_users(ConnectedList, Session_Manager),
                    ConnectedList
            end;
        true ->
            Pid ! unknown_user,
            Connected
    end.

register_user(NickName, Users, Pid) ->
    Exists = check_nick_exists(NickName, Users),
    if
        Exists == true ->
            Pid ! false,
            Users;
        true ->
            Userslist = lists:append(Users, [NickName]),
            Pid ! true,
            Userslist
    end.

check_nick_exists(_, []) ->
    false;
check_nick_exists(NickName, [H|T]) ->
    if
        NickName == H ->
            true;
        true ->
            check_nick_exists(NickName, T)
    end.
check_user_in_room(NickName, Tuple_room) ->
    Users = element(2, Tuple_room),
    Exists = check_nick_exists(NickName, Users),
    Exists.