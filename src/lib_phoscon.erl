%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_phoscon).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include("device.spec").
%% --------------------------------------------------------------------

%% External exports
-export([

%	 all_light_maps/3,
%	 all_sensor_maps/3,
	 
	 what_devices/4,
	 get_maps/4,
	 set_state/7
	]). 


%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_maps("sensors",ConbeeAddr,ConbeePort,Crypto)->
    get("sensors",ConbeeAddr,ConbeePort,Crypto);
get_maps("lights",ConbeeAddr,ConbeePort,Crypto)->
    get("lights",ConbeeAddr,ConbeePort,Crypto);
get_maps(NoMatch,ConbeeAddr,ConbeePort,Crypto)->
    [{error,[NoMatch,?MODULE,?LINE]}].

get(DeviceType,ConbeeAddr,ConbeePort,Crypto)->
    {ok, ConnPid} = gun:open(ConbeeAddr,ConbeePort),
    Cmd="/api/"++Crypto++"/"++DeviceType,
    Ref=gun:get(ConnPid,Cmd),
    MapsList = case gun:await_body(ConnPid, Ref) of
		   {ok,Body}->
		       jsx:decode(Body,[])
	       end,
    ok=gun:close(ConnPid),
    MapsList.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
what_devices("lights",Ip,Port,Crypto)->
    Maps=get_maps(<<"lights">>,Ip,Port,Crypto),
    Maps;
what_devices("sensors",Ip,Port,Crypto)->
    Maps=get_maps(<<"sensors">>,Ip,Port,Crypto),
    Maps.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_state(Id,Key,Value,DeviceType,Ip,Port,Crypto)->
    Cmd="/api/"++Crypto++"/"++DeviceType++"/"++Id++"/state",
    Body=jsx:encode(#{Key => Value}),
    {ok, ConnPid} = gun:open(Ip,Port),
    StreamRef = gun:put(ConnPid, Cmd, 
			[{<<"content-type">>, "application/json"}],Body),
    Result=lib_conbee:get_reply(ConnPid,StreamRef),
    ok=gun:close(ConnPid),
    Result.
