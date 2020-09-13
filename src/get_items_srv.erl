%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(get_items_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2,
    code_change/3
]).
-export([
    get_breakdown/1, get_breakdown/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    cached_items = #{} :: map()
}).

-record(breakdown_request, {
    name = <<>> :: binary(),
    quantity = 0 :: non_neg_integer()
}).

-record(uncache, {
    key = erlang:error(undef) :: {Name :: binary(), Quantity :: non_neg_integer()}
}).

%%%===================================================================
%%% Public API
%%%===================================================================

get_breakdown(Name) ->
    get_breakdown(Name, 1).

get_breakdown(Name, Quantity) when is_binary(Name) andalso 0 < Quantity->
    call(#breakdown_request{name = Name, quantity = Quantity}).

%%%===================================================================
%%% Gen Server Accessors
%%%===================================================================

call(Request) ->
    call(Request, 5000).

call(Request, Timeout) ->
    gen_server:call(?SERVER, Request, Timeout).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(BR = #breakdown_request{}, _From, State0 = #state{}) ->
    lager:debug("got BreakDown Request ~p~n", [BR]),
    {BreakDown, State1} = fetch_and_cache(BR, State0),
    Reply = 
        case BreakDown of
            M when map_size(M) =:= 0 ->
                error;
            M ->
                {ok, M}
        end,
    {reply, Reply, State1};
handle_call(_Request, _From, State = #state{}) ->
    {reply, error, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(#uncache{key = Key}, State0 = #state{cached_items = CachedItems}) ->
    lager:info("Uncaching Key '~p'", [Key]),
    State1 = State0#state{cached_items = maps:remove(Key, CachedItems)},
    {noreply, State1};
handle_info(_Info, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

fetch_and_cache(#breakdown_request{name = Name, quantity = Quantity}, State0 = #state{cached_items = CachedItems}) ->
    LowerName = gi_utils:bin_to_lower(Name),
    Key = {LowerName, Quantity},
    BreakDown =
        case maps:get(Key, CachedItems, undefined) of
            undefined ->
                breakdown(LowerName, Quantity);
            {BreakDown_, Timer} ->
                lager:info("Using cached value for Key '~p'", [Key]),
                erlang:cancel_timer(Timer),
                BreakDown_
        end,
    TimerRef = erlang:send_after(gi_config:get_cache_time(), self(), #uncache{key = Key}),
    State1 = State0#state{
        cached_items = CachedItems#{
            {LowerName, Quantity} => {BreakDown, TimerRef}
        }
    },
    {BreakDown, State1}.

-spec breakdown(Name :: binary(), Quantity :: non_neg_integer()) -> #{binary() => non_neg_integer()}.
breakdown(Name, Quantity) when Quantity > 0 ->
    case breakdown_(Name, Quantity) of
        [{Name, Quantity}] ->
            #{};
        Items ->
            quantise_breakdown(lists:flatten(Items), #{})
    end.

breakdown_(Name, Quantity) ->
    case gi_config:get_recipe_mapping(Name) of
        undefined ->
            [{Name, Quantity}];
        {Requirements, OutputQuantity} ->
            [breakdown_(N, Q * math:ceil(Quantity/OutputQuantity)) || {N, Q} <- Requirements]
    end.

quantise_breakdown([], Acc) ->
    Acc;
quantise_breakdown([{Name, Quantity}|Tail], Acc0) ->
    Counter0 = maps:get(Name, Acc0, 0),
    Counter1 = Counter0 + Quantity,
    Acc1 = maps:put(Name, Counter1, Acc0),
    quantise_breakdown(Tail, Acc1).