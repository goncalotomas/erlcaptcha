%%%-------------------------------------------------------------------
%% @doc erlcaptcha top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlcaptcha_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    RestartStrategy = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [
        #{
            id => erlcaptcha,
            start => {erlcaptcha, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
