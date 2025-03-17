-module(coffee_fsm).
-behavior(gen_fsm).

-export([start_link/0, stop/0]).
-export([init/1, terminate/1, handle_event/3]). %% Callback functions
-export([selection/2, payment/2, remove/2]).	%% States
-export([americano/0, cappuccino/0, tea/0, espresso/0, %% Client functions
	 pay/1, cancel/0, cup_removed/0]).

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	hw:reboot(),
	hw:display("Make Your Selection", []),
	process_flag(trap_exit, true),
	{ok, selection, []}.

stop() ->
	gen_fsm:stop().

terminate({_Type, _Price, Paid}) ->
	hw:return_change(Paid),
	ok.

tea() -> gen_fsm:send_event(?MODULE, {selection, tea, 100}).
espresso() -> gen_fsm:send_event(?MODULE, {selection, espresso, 100}).
americano() -> gen_fsm:send_event(?MODULE, {selection, american, 150}).
cappuccino() -> gen_fsm:send_event(?MODULE, {selection, cappuccion, 150}).
pay(Coin) -> gen_fsm:send_event(?MODULE, {pay, Coin}).
cancel() -> gen_fsm:send_event(?MODULE, cancel).
cup_removed() -> gen_fsm:send_event(?MODULE, cup_removed).

handle_event(event, selection, LoopData) ->
	selection(event, LoopData);
handle_event(event, payment, LoopData) ->
	payment(event, LoopData);
handle_event(event, remove, LoopData) ->
	remove(event, LoopData).


selection({selection, Type, Price}, _LoopData) ->
	hw:display("Please Pay:~w", [Price]),
	{next_state, payment, {Type, Price, 0}};
selection({pay, Coin}, LoopData) ->
	hw:return_change(Coin),
	{next_state, selection, LoopData};
selection(_Other, LoopData) ->
	{next_state, selection, LoopData}.

payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid < Price ->
	NewPaid = Coin + Paid,
	hw:display("Please pay:~w",[Price-NewPaid]),
	{next_state, payment, {Type, Price, NewPaid}};
payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid >= Price ->
	NewPaid = Coin + Paid,
	hw:display("Preparing Drink.",[]),
	hw:return_change(NewPaid - Price),
	hw:drop_cup(), hw:prepare(Type),
	hw:display("Remove Drink",[]),
	{next_state, remove, null};
payment(cancel, {_Type, _Price, Paid}) ->
	hw:display("Make your selection", []),
	hw:return_change(Paid),
	{next_state, selection, null};
payment(_Other, LoopData) ->
	{next_state, payment, LoopData}.

remove(cup_removed, LoopData) ->
	hw:display("Make your selection", []),
	{next_state, selection, LoopData};
remove({pay, Coin}, LoopData) ->
	hw:return_change(Coin),
	{next_state, remove, LoopData};
remove(_Other, LoopData) ->
	{next_state, remove, LoopData}.
