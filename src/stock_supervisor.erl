%% @author William Granli
%% @doc 
%% 	OTP supervisor that supervises our two gen_servers: control_unit and database_server
%% @end

-module(stock_supervisor).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []),
	scheduler:start().

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1,
	MaxSecondsBetweenRestarts = 5,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = permanent,
	Shutdown = 2000,
	Type = worker,

	Control_Unit = {control_unit, {control_unit, start_link, []},
		Restart, Shutdown, Type, [control_unit]},

	Database_Server = {database_server,{database_server, start_link, []},
		Restart, Shutdown, Type,[database_server]},
	
	Children = [Control_Unit, Database_Server],
		{ok, {SupFlags, Children}}.