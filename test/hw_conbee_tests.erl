%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(hw_conbee_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(TestHost,"c202").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=hw_conbee_test(),
   % ok=test_1(),
   % ok=test_2(),
    
				
     
  
    init:stop(),
    timer:sleep(2000),
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test_2()-> %  check the prototype
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    % "switch_prototype"
%	"temp_prototype"
    Switch=rd:rpc_call(hw_conbee,hw_conbee,get,["switch_prototype"],2000),
    io:format("Switch ~p~n",[{Switch,?MODULE,?FUNCTION_NAME}]),
    Temp=rd:rpc_call(hw_conbee,hw_conbee,get,["temp_prototype"],2000),
    io:format("Temp ~p~n",[{Temp,?MODULE,?FUNCTION_NAME}]),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
hw_conbee_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
     AllSensor=hw_conbee:get_all_device_info("sensors"),
    io:format("AllSensors ~p~n",[{AllSensors,?MODULE,?FUNCTION_NAME}]),
    AllLights=hw_conbee:get_all_device_info("lights"),
    io:format("AllLights ~p~n",[{AllLights,?MODULE,?FUNCTION_NAME}]),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    application:start(etcd),
    
    application:load(hw_conbee),
    application:set_env([{hw_conbee,[{test_host,?TestHost}]}]),
    application:start(hw_conbee),

    {ok,[{conbee,ConbeeConfig}]}=etcd_host:get_appl_config(?TestHost),
    {conbee_addr,ConbeeAddr}=lists:keyfind(conbee_addr,1,ConbeeConfig),
    {conbee_port,ConbeePort}=lists:keyfind(conbee_port,1,ConbeeConfig),
    {conbee_key,ConbeeKey}=lists:keyfind(conbee_key,1,ConbeeConfig),
    "172.17.0.2"=ConbeeAddr,
    80=ConbeePort,
    "24E285605D"=ConbeeKey,
   
    ok.
