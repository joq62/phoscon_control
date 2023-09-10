%%%-------------------------------------------------------------------
%% @doc phoscon_control public API
%% @end
%%%-------------------------------------------------------------------

-module(phoscon_control_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    phoscon_control_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
