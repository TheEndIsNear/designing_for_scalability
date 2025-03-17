-module(test_fsm).
-compile(export_all).
-behavior(gen_fsm).

start_link(TimerMs, Options) ->
	gen_fsm:start_link(?MODULE, TimerMs, Options).
start(TimerMs, Options) ->
	gen_fsm:start(?MODULE, TimerMs, Options).

init(0) ->
	{stop, stopped};
init(1) ->
	{next_state, selection, []};
init(TimerMs) ->
	timer:sleep(TimerMs),
	ignore.

