-module(hlr).
-export([new/0, attach/1, detach/0, lookup_id/1, lookup_ms/1]).

new() ->
	ets:new(msidn2pid, [public, named_table]),
	ets:new(pid2msidn, [public, named_table]),
	ok.

attach(Ms) ->
	ets:insert(msidn2pid, {Ms, self()}),
	ets:insert(pid2msidn, {self(), Ms}).

detach() ->
	case ets:lookup(pid2msidn, self()) of
		[{Pid, Ms}] ->
			ets:delete(pid2msidn, Pid),
			ets:delete(msidn2pid, Ms);
		[] ->
			ok
	end.

lookup_id(Ms) ->
	case ets:lookup(msidn2pid, Ms) of
		[] -> {error, invalid};
		[{Ms, Pid}] -> {ok, Pid}
	end.

lookup_ms(Pid) ->
	case ets:lookup(pid2msidn, Pid) of
		[] -> {error, invalid};
		[{Pid, Ms}] -> {ok, Ms}
	end.
