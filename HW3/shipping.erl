% Justin Chen
% I pledge my honor that I have abided by the Stevens Honor System

-module(shipping).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    % Shipping state has a ships array. We look through the ships array to find something with Ship_ID.
    Matches_ID = fun (Ship) -> Ship#ship.id == Ship_ID end,
    Ship = lists:filter(Matches_ID, Shipping_State#shipping_state.ships),   % Record type

    if
        Ship == [] ->
            throw(error);
        true ->
            % @see https://stackoverflow.com/questions/69095161/in-erlang-where-are-the-builtin-head-and-tail-functions
            hd(Ship)
    end.

get_container(Shipping_State, Container_ID) ->
    % Shipping state has a containers array. We look through the containers array to find something with Container_ID.
    Matches_ID = fun (Container) -> Container#container.id == Container_ID end,
    Container = lists:filter(Matches_ID, Shipping_State#shipping_state.containers),

    if 
        % If we found no matches
        Container == [] ->
            throw(error);
        true ->
            hd(Container)
    end.

get_port(Shipping_State, Port_ID) ->
    % Shipping state has a ports array. We look through the ports array to find something with Port_ID.
    Matches_ID = fun (Port) -> Port#port.id == Port_ID end,
    Port = lists:filter(Matches_ID, Shipping_State#shipping_state.ports),
    
    if 
        % If we found no matches
        Port == [] ->
            throw(error);
        true ->
            hd(Port)
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    % First, filter all the occupied docks so you only have the ship locations tuple for a given port ID
    % Because we used filter, if the ID doesn't exist, then this will still return an empty list
    Matches_ID = fun ({Curr_Port_ID, _Dock_ID, _Ship_ID}) -> Port_ID == Curr_Port_ID end,
    Docks_For_Curr_ID = lists:filter(Matches_ID, Shipping_State#shipping_state.ship_locations),

    % Now, map all the ship locations tuples so that we only get the Dock ID
    Only_Get_Dock_ID = fun({_Curr_Port_ID, Dock_ID, _Ship_ID}) -> Dock_ID end,
    Occupied_Docks = lists:map(Only_Get_Dock_ID, Docks_For_Curr_ID),
    Occupied_Docks.

get_ship_location(Shipping_State, Ship_ID) ->
    % Ship locations tuple has PortID and DockID we can grab
    Matches_ID = fun ({_Port_ID, _Dock_ID, Current_Ship_ID}) -> Current_Ship_ID == Ship_ID end,
    Ship_Tuples = lists:filter(Matches_ID, Shipping_State#shipping_state.ship_locations),

    % If ship doesn't exist, errors
    if
        Ship_Tuples == [] ->
            throw(error);
        true ->
            % Now, only grab Port ID and Dock ID
            Get_Important_Stuff = fun({Port_ID, Dock_ID, _BooFuzz}) -> {Port_ID, Dock_ID} end,
            Ship_Locations = lists:map(Get_Important_Stuff, Ship_Tuples),

            % A ship can only be in one location a once
            hd(Ship_Locations)
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    % Convert the container IDs to Container objects
    % The skibidi thing is that get_container will error with us
    Fetch_Container = fun (ID) -> get_container(Shipping_State, ID) end,
    Containers = lists:map(Fetch_Container, Container_IDs),

    % Now, convert the container objects into container weights
    Get_Container_Weights = fun (Container) -> Container#container.weight end,
    Weights = lists:map(Get_Container_Weights, Containers),

    % Now, sum all the weights
    lists:sum(Weights).

get_ship_weight(Shipping_State, Ship_ID) ->
    % Grab the ship object for the ship ID
    % It does nothing. However, this will error for us if the ship doesn't exist
    _Ship = get_ship(Shipping_State, Ship_ID),

    % Grab the list of container IDs that correspond to the containers on the ship
    % Shipping_State's ship_inventory is a map
    Container_IDs = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),

    % Now, grab their weights
    get_container_weight(Shipping_State, Container_IDs).

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    % Containers in Container_ID  (port inventory) --> Moved to Ship_ID (ship inventory) ✅
    % Ship_ID doesn't exist --> Error ✅
    % Overcapacity --> Error ✅
    % Container_ID doesn't exist in shipping state --> Error ✅
    % Container not in port --> Error Container_IDs ✅

    % Step 1: Check if Ship_ID exists. If not, this will error.
    % Also grab the ship inventory. We'll need this later
    Ship = get_ship(Shipping_State, Ship_ID),
    Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),

    % Step 2: Get port and dock that the ship is on. We'll need these in order to remove carts from the port
    % Also grab all the containers in the same port as the shipping ID. We'll need that later
    {Port_ID, _Dock_ID} = get_ship_location(Shipping_State, Ship_ID),
    Port_ID_Containers = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),

    % Step 3: Make sure all the containers are in the shipping state. Or else, we error
    % We will just loop through everything and get container. If we don't, skibidi will error
    Get_Container_By_ID = fun (ID) -> get_container(Shipping_State, ID) end,
    _Skibidi = lists:map(Get_Container_By_ID, Container_IDs),

    % Step 4: Make sure all the containers are in the same port as the ship. Else, we error
    % We will first filter all the container IDs that are in the same port as the Ship. Basically, container ID must also be present in Port_ID_Containers
    % If the two container ID array and the filtered ID array are not the same, that means there's some container IDs that are not in the port
    In_Same_Port = fun (ID) -> lists:member(ID, Port_ID_Containers) end,
    Container_ID_In_Same_Port = lists:filter(In_Same_Port, Container_IDs),

    if
        Container_ID_In_Same_Port =/= Container_IDs -> % If there's container IDs not in Port_ID_Containers, error
            throw(error);
        true ->        
            
            % Step 5: Check for overcapacity
            if
                length(Ship_Inventory) + length(Container_ID_In_Same_Port) > Ship#ship.container_cap ->
                    throw(error);
                true ->
                    % Step 6: Change the shipping state
                    load_ship_helper(Shipping_State, Container_ID_In_Same_Port, Ship_ID, Port_ID, Ship_Inventory, Port_ID_Containers)
            end
    end.

load_ship_helper(Shipping_State, Container_ID_In_Same_Port, Ship_ID, Port_ID, Old_Ship_Inv, Old_Port_Inv) ->
    % We need to first update ship inventory and the shipping state inv
    Updated_Ship_Inv = Old_Ship_Inv ++ Container_ID_In_Same_Port,
    Updated_Ship_Map = maps:put(Ship_ID, Updated_Ship_Inv, Shipping_State#shipping_state.ship_inventory),

    % Then we need to update the port inventory and the shipping state inv
    % Remember we need to subtract the containers from the port
    Updated_Port_Inv = lists:subtract(Old_Port_Inv, Container_ID_In_Same_Port),
    Updated_Port_Map = maps:put(Port_ID, Updated_Port_Inv, Shipping_State#shipping_state.port_inventory),

    % Then we return the gigantic new shipping state
    % Most of the stuff does not change
    {
        ok, #shipping_state{
            ships = Shipping_State#shipping_state.ships,
            containers = Shipping_State#shipping_state.containers,
            ports = Shipping_State#shipping_state.ports,
            ship_locations = Shipping_State#shipping_state.ship_locations,
            ship_inventory = Updated_Ship_Map,
            port_inventory = Updated_Port_Map
        }
    }.

unload_ship_all(Shipping_State, Ship_ID) ->
    % Returns a shipping state where all containers on a ship is offloaded onto the port they're docked on ✅
    % If overcapacity --> error ✅
    % If ship ID doesn't exist --> error ✅
    
    % Step 1: Check if Ship_ID exists. If not, this will error.
    % Also grab the ship inventory. We'll need this later
    _Ship = get_ship(Shipping_State, Ship_ID),
    Ship_Inv = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),

    % Step 2: Get port that the ship is on. We'll need these in order to remove carts from the port
    % Also grab the port inventory of the Port the ship is docked at. We'll need that later
    {Port_ID, _Dock_ID} = get_ship_location(Shipping_State, Ship_ID),
    Port_Inv = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),

    % Step 3: Grab port maximum capacity of the port the ship is docked at
    Port = get_port(Shipping_State, Port_ID),
    Max_Port_Capacity = Port#port.container_cap,

    if
        % Step 4: Check overcapacity
        length(Ship_Inv) + length(Port_Inv) > Max_Port_Capacity ->
            throw(error);
        true ->
            % Step 5: Load da boat!
            unload_all_ship_helper(Shipping_State, Ship_ID, Port_ID, Ship_Inv, Port_Inv)
    end.

unload_all_ship_helper(Shipping_State, Ship_ID, Port_ID, Old_Ship_Inv, Old_Port_Inv) ->
    % We first need to update the port inventory to include literally all the ship containers
    Updated_Port_Inv = Old_Port_Inv ++ Old_Ship_Inv,

    % We then need to update the map
    Updated_Port_Map = maps:put(Port_ID, Updated_Port_Inv, Shipping_State#shipping_state.port_inventory),

    % We also need to update the map for the ship so the Ship_ID has no containers
    Updated_Ship_Map = maps:put(Ship_ID, [], Shipping_State#shipping_state.ship_inventory),

    % Then we return the gigantic new shipping state
    {
        ok, #shipping_state{
            ships = Shipping_State#shipping_state.ships,
            containers = Shipping_State#shipping_state.containers,
            ports = Shipping_State#shipping_state.ports,
            ship_locations = Shipping_State#shipping_state.ship_locations,
            ship_inventory = Updated_Ship_Map,
            port_inventory = Updated_Port_Map
        }
    }.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    % Containers in Container_ID  (ship inventory) --> Moved to Port_ID (port inventory) ✅
    % Ship_ID doesn't exist --> Error ✅
    % Overcapacity --> Error ✅
    % Container_ID doesn't exist in shipping state --> Error ✅
    % Container not in ship --> Error Container_IDs ✅

    % Step 1: Check if Ship_ID exists. If not, this will error.
    % Also grab the ship inventory. We'll need this later
    _Ship = get_ship(Shipping_State, Ship_ID),
    Ship_Inventory = maps:get(Ship_ID, Shipping_State#shipping_state.ship_inventory),

    % Step 2: Get port and dock that the ship is on. We'll need these in order to remove carts from the port
    % Also grab the port inventory of the port that the ship is docked on right now 
    {Port_ID, _Dock_ID} = get_ship_location(Shipping_State, Ship_ID),
    Port_Inventory = maps:get(Port_ID, Shipping_State#shipping_state.port_inventory),

    % Step 3: Make sure all the containers are in the shipping state to begin with. Or else, we error
    % We will just loop through everything and get container. If we don't, hawk tuah will error
    Get_Container_By_ID = fun (ID) -> get_container(Shipping_State, ID) end,
    _HawkTuah = lists:map(Get_Container_By_ID, Container_IDs),

    % Step 4: Make sure all the containers are on the ship (in the ship's inventory)
    % Just realized there was a is_sublist command. Oop I think it's too late to use that :(
    % We will first filter all the container IDs to only keep the ones that are on the ship
    % If the container ID array and the filtered ID array are not the same, that means there's some container IDs that are not in the ship
    On_Ship = fun (ID) -> lists:member(ID, Ship_Inventory) end,
    Container_ID_On_Ship = lists:filter(On_Ship, Container_IDs),

    if
        Container_ID_On_Ship =/= Container_IDs -> % If there's container IDs not on the ship (heh, not in Container_ID_On_Ship), error
            throw(error);
        true ->        
            
            % Step 5: Check for overcapacity
            % We'll need to grab port maximum capacity of the port the ship is docked at
            Port = get_port(Shipping_State, Port_ID),
            Max_Port_Capacity = Port#port.container_cap,

            if
                length(Ship_Inventory) + length(Container_ID_On_Ship) > Max_Port_Capacity ->
                    throw(error);
                true ->
                    % Step 6: Change the shipping state
                    unload_ship_helper(Shipping_State, Ship_ID, Port_ID, Ship_Inventory, Port_Inventory, Container_ID_On_Ship)
            end
    end.

unload_ship_helper(Shipping_State, Ship_ID, Port_ID, Old_Ship_Inv, Old_Port_Inv, Containers_To_Unload) ->
    % We need to first update ship inventory and the shipping state inv
    % Remember we took stuff off the ship
    Updated_Ship_Inv = lists:subtract(Old_Ship_Inv, Containers_To_Unload),
    Updated_Ship_Map = maps:put(Ship_ID, Updated_Ship_Inv, Shipping_State#shipping_state.ship_inventory),

    % Then we need to update the port inventory and the shipping state inv
    % Remember we added stuff to the port
    Updated_Port_Inv = Old_Port_Inv ++ Containers_To_Unload,
    Updated_Port_Map = maps:put(Port_ID, Updated_Port_Inv, Shipping_State#shipping_state.port_inventory),

    % Then we return the gigantic new shipping state
    % Most of the stuff does not change
    {
        ok, #shipping_state{
            ships = Shipping_State#shipping_state.ships,
            containers = Shipping_State#shipping_state.containers,
            ports = Shipping_State#shipping_state.ports,
            ship_locations = Shipping_State#shipping_state.ship_locations,
            ship_inventory = Updated_Ship_Map,
            port_inventory = Updated_Port_Map
        }
    }.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    % Changes a ship's port and dock to a new location
    % If port or dock is occupied --> error ✅
    % Ship_ID does not exist --> error ✅
    % If port doesn't exist --> error ✅
    % If dock not valid for port --> error ✅

    % Step 1: Check if Ship_ID exists. If not, this will error.
    _WeArePOGGING = get_ship(Shipping_State, Ship_ID),
    
    % Step 2: Check if port is valid
    % Also grab valid docks for the port. This will error if port doesn't exist
    % Also grab the occupied docks for the port. We'll need it later
    Port = get_port(Shipping_State, Port_ID),
    Port_Docks = Port#port.docks,
    Occupied_Docks = get_occupied_docks(Shipping_State, Port_ID),

    % Step 3: Check if dock is valid for port
    % For some reason Erlang is being stupid and won't stop complaining about illegal guard expressions
    % Stupid *** language it's 2025 be better how tf do you make less sense than C
    % This is the problem with all these "functional languages" 
    % Anyways end of rant we're using case statements

    case lists:member(Dock, Port_Docks) of
        true ->
            % Step 4: Check if dock is occupied. If they are, error
            case lists:member(Dock, Occupied_Docks) of
                true ->
                    throw(error);
                false ->
                    % Step 5: If we are all good, change a ship's port and dock to the new location
                    set_sail_helper(Shipping_State, Ship_ID, Port_ID, Dock)
            end;
        false ->
            % Part of step 3, but error if Dock name is not valid for the port
            throw(error)
    end.

set_sail_helper(Shipping_State, Ship_ID, Port_ID, Dock) ->
    % Create a new ships location list/array with the updated PortID and Dock
    New_Ship_Locations = create_updated_ship_location(Shipping_State#shipping_state.ship_locations, Ship_ID, Port_ID, Dock),

    % Then we return the gigantic new shipping state
    % Most of the stuff does not change
    {
        ok, #shipping_state{
            ships = Shipping_State#shipping_state.ships,
            containers = Shipping_State#shipping_state.containers,
            ports = Shipping_State#shipping_state.ports,
            ship_locations = New_Ship_Locations,
            ship_inventory = Shipping_State#shipping_state.ship_inventory,
            port_inventory = Shipping_State#shipping_state.port_inventory
        }
    }.
    
create_updated_ship_location([], _Target_Ship_ID, _New_Port_ID, _New_Dock) -> [];
create_updated_ship_location([{Port_ID, Dock, Ship_ID} | Tail], Target_Ship_ID, New_Port_ID, New_Dock) ->
    if
        Ship_ID == Target_Ship_ID ->
            [{New_Port_ID, New_Dock, Target_Ship_ID} | create_updated_ship_location(Tail, Target_Ship_ID, New_Port_ID, New_Dock)];
        true ->
            [{Port_ID, Dock, Ship_ID} | create_updated_ship_location(Tail, Target_Ship_ID, New_Port_ID, New_Dock)]
    end.

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
