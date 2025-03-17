-module(coffee_statem).
-behavior(gen_statem).

-export([start_link/0, stop/0]).
-export([init/1, terminate/3, callback_mode/0]). %% Callback functions
-export([selection/3, payment/3, remove/3]).	%% States
-export([americano/0, cappuccino/0, tea/0, espresso/0, %% Client functions
	 pay/1, cancel/0, cup_removed/0]).

start_link() ->
	gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_statem:stop(?MODULE).

init([]) ->
	hw:reboot(),
	hw:display("Make Your Selection", []),
	%process_flag(trap_exit, true),
	{ok, selection, []}.

terminate(_Reason, _State, {_Type, _Price, Paid} = LoopData) ->
	hw:return_change(Paid),
	{stop, normal, LoopData};
terminate(_Reason, _State, LoopData) ->
	{stop, normal, LoopData}.

callback_mode() -> state_functions.

tea() -> gen_statem:call(?MODULE, {selection, tea, 100}).
espresso() -> gen_statem:call(?MODULE, {selection, espresso, 100}).
americano() -> gen_statem:call(?MODULE, {selection, american, 150}).
cappuccino() -> gen_statem:call(?MODULE, {selection, cappuccion, 150}).
pay(Coin) -> gen_statem:call(?MODULE, {pay, Coin}).
cancel() -> gen_statem:call(?MODULE, cancel).
cup_removed() -> gen_statem:call(?MODULE, cup_removed).

selection({call, From}, {selection, Type, Price}, _LoopData) ->
	hw:display("Please Pay:~w", [Price]),
	{next_state, payment, {Type, Price, 0}, [{reply, From, selection}]};
selection({call, From}, {pay, Coin}, LoopData) ->
	hw:return_change(Coin),
	{next_state, selection, LoopData, [{reply, From, selection}]};
selection({call, From}, _Other, LoopData) ->
	{next_state, selection, LoopData, [{reply, From, selection}]}.

payment({call, From}, {pay, Coin}, {Type, Price, Paid}) when Coin+Paid < Price ->
	NewPaid = Coin + Paid,
	hw:display("Please pay:~w",[Price-NewPaid]),
	{next_state, payment, {Type, Price, NewPaid}, [{reply, From, payment}]};
payment({call, From}, {pay, Coin}, {Type, Price, Paid}) when Coin+Paid >= Price ->
	NewPaid = Coin + Paid,
	hw:display("Preparing Drink.",[]),
	hw:return_change(NewPaid - Price),
	hw:drop_cup(), hw:prepare(Type),
	hw:display("Remove Drink",[]),
	{next_state, remove, null, [{reply, From, payment}]};
payment({call, From},cancel, {_Type, _Price, Paid}) ->
	hw:display("Make your selection", []),
	hw:return_change(Paid),
	{next_state, selection, null, [{reply, From, payment}]};
payment({call, From},_Other, LoopData) ->
	{next_state, payment, LoopData, [{reply, From, payment}]}.

remove({call, From},cup_removed, LoopData) ->
	hw:display("Make your selection", []),
	{next_state, selection, LoopData, [{reply, From, remove}]};
remove({call, From},{pay, Coin}, LoopData) ->
	hw:return_change(Coin),
	{next_state, remove, LoopData, [{reply, From, remove}]};
remove({call, From},_Other, LoopData) ->
	{next_state, remove, LoopData, [{reply, From, remove}]}.
