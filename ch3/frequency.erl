-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() -> register(frequency, spawn(frequency, init, [])).

init() ->
	Frequencies = {get_frequencies(), []},
	loop(Frequencies).

get_frequencies() -> [10, 11, 12, 13, 14, 15].

loop(Frequencies) ->
	receive 
	{request, Pid, allocate} ->
		{NewFrequencies, Reply} = allocate(Frequencies, Pid),
		reply(Pid, Reply),
		loop(NewFrequencies);
	{request, Pid, {deallocate, Freq}} ->
		{NewFrequencies, Reply} = deallocate(Freq, Pid),
		reply(Pid, Reply),
		loop(NewFrequencies);
	{request, Pid, stop} ->
		reply(Pid, ok)
	end.

% Internal Server Functions
allocate({[], Allocated}, _Pid) ->
	{{[], Allocated}}, {error, no_frequencies};
allocate({[Freq|Free], Allocated}, Pid) ->
	{{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
	NewAllocated = lists:keydelete(Freq, 1, Allocated),
	{[Freq|Free], NewAllocated}.

% client functions
stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
	frequency ! {request, self(), Message},
	receive {reply, Reply} -> Reply end.

reply(Pid, Reply) ->
	Pid ! {reply, Reply}.
