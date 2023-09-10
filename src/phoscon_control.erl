%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% Description :
%%% 
%%% Created :
%%% -------------------------------------------------------------------
-module(phoscon_control).
  
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-include("log.api").
% -include("device.spec").

-define(ConbeeContainer,"deconz").
-define(SERVER,?MODULE).

%% External exports
-export([
	 %% basic
	 set_state/4,
	 set_config/4,
	 get_maps/0,
	 get_maps/1
	
	 
	]).


-export([
	 ping/0,
	 start_link/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------

-record(state,{device_info,
	       ip_addr,
	       ip_port,
	       crypto
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop()-> gen_server:call(?SERVER, {stop},infinity).


%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_maps()->
    gen_server:call(?SERVER, {get_maps},infinity). 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_maps(DeviceType)->
    gen_server:call(?SERVER, {get_maps,DeviceType},infinity). 


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_state(Id,Key,Value,DeviceType)->
    gen_server:call(?SERVER, {set_state,Id,Key,Value,DeviceType},infinity). 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
set_config(Id,Key,Value,DeviceType)->
    gen_server:call(?SERVER, {set_config,Id,Key,Value,DeviceType},infinity). 

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ping() ->
    gen_server:call(?SERVER, {ping}).


%% cast

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,HostName}=net:gethostname(),
%    {ok,[{conbee,ConbeeConfig}]}=etcd_host:get_appl_config(HostName),

    {ok,[{conbee,ConbeeConfig}]}=rpc:call('etcd_c201@c201',etcd_host,get_appl_config,[HostName],5000),
    {conbee_addr,ConbeeAddr}=lists:keyfind(conbee_addr,1,ConbeeConfig),
    {conbee_port,ConbeePort}=lists:keyfind(conbee_port,1,ConbeeConfig),
    {conbee_key,ConbeeKey}=lists:keyfind(conbee_key,1,ConbeeConfig),
    application:ensure_all_started(gun),
    
    os:cmd("docker restart "++?ConbeeContainer),
    timer:sleep(5*1000),
    
    ?LOG_NOTICE("Server started ",["Servere started",node(),
				   ip_addr,ConbeeAddr,
				   ip_port,ConbeePort,
				   crypto,ConbeeKey]),
   
    {ok, #state{device_info=undefined,
	        ip_addr=ConbeeAddr,
		ip_port=ConbeePort,
		crypto=ConbeeKey}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({get_maps},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
 
    LightsMaps=lib_phoscon:get_maps("lights",ConbeeAddr,ConbeePort,Crypto),
    SensorsMaps=lib_phoscon:get_maps("sensors",ConbeeAddr,ConbeePort,Crypto),
    Reply=[{"lights",LightsMaps},{"sensors",SensorsMaps}],
    {reply, Reply, State};


%%---------------------------------------------------------------------
%% Lights 
%%---------------------------------------------------------------------
handle_call({get_maps,"lights"},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
 
    Reply=lib_phoscon:get_maps("lights",ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

handle_call({set_state,Id,Key,Value,"lights"},_From, State) ->
    DeviceType="lights",
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_phoscon:set_state(Id,Key,Value,DeviceType,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};


%%---------------------------------------------------------------------
%%  Sensors 
%%---------------------------------------------------------------------
handle_call({get_maps,"sensors"},_From, State) ->
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,

    Reply=lib_phoscon:get_maps("sensors",ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

handle_call({set_state,Id,Key,Value,"sensors"},_From, State) ->
    DeviceType="sensors",
    ConbeeAddr=State#state.ip_addr,
    ConbeePort=State#state.ip_port,
    Crypto=State#state.crypto,
    Reply=lib_phoscon:set_state(Id,Key,Value,DeviceType,ConbeeAddr,ConbeePort,Crypto),
    {reply, Reply, State};

%%---------------------------------------------------------------------
%%  General 
%%---------------------------------------------------------------------


handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    ?LOG_WARNING("Unmatched signal",[Request]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?LOG_WARNING("Unmatched signal",[Msg]),
    
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_info({gun_up,_,http}, State) -> 
    {noreply, State};


handle_info({gun_response,_,_,_,_,_}, State) -> 
    {noreply, State};

handle_info(timeout, State) -> 
    io:format("timeout ~p~n",[{?MODULE,?LINE}]), 
    
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
