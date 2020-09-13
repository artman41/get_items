%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2020 11:04 PM
%%%-------------------------------------------------------------------
-module(get_items_endpoint).
-author("artman41").

%% API
-export([init/2]).

init(Req0, Opts) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            QueryParams = get_query_params(Req0),
            Req1 = handle_request(Req0, QueryParams),
            {ok, Req1, Opts};
        _Method ->
            Req1 = cowboy_req:reply(500, #{
                <<"content-type">> => <<"text/plain; charset=utf-8">>
            }, <<"Invalid Method">>, Req0),
            {stop, Req1, Opts}
    end.

handle_request(Req0, QueryParams) ->
    Body = 
        case maps:get(<<"item">>, QueryParams, undefined) of
            undefined ->
                build_knowledge_bin();
            Item ->
                QuantityBin = maps:get(<<"quantity">>, QueryParams, <<"1">>),
                Breakdown = get_items_srv:get_breakdown(Item, binary_to_integer(QuantityBin)),
                build_breakdown_bin(Breakdown)
        end,
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    }, Body, Req0).

-spec get_query_params(cowboy_req:req()) -> map().
get_query_params(Req) ->
    maps:from_list(cowboy_req:parse_qs(Req)).

build_knowledge_bin() ->
    Mappings = gi_config:get_recipe_mappings(),
    RecipeMappingsList = maps:to_list(Mappings),
    SortedRecipeMappingsList = lists:sort(RecipeMappingsList),
    BodyIOList = [[Name, ": ", integer_to_binary(Quantity), $\n, build_knowledge_bin_(Requirements, []), $\n] || {Name, {Requirements, Quantity}} <- SortedRecipeMappingsList],
    iolist_to_binary(BodyIOList).

build_knowledge_bin_([], Acc) ->
    Acc;
build_knowledge_bin_([{Name, Quantity} | Tail], Acc0) ->
    Acc1 = ["    - ", Name, ": ", integer_to_list(Quantity), $\n | Acc0],
    build_knowledge_bin_(Tail, Acc1).

build_breakdown_bin(error) ->
    <<"Got error attempting request">>;
build_breakdown_bin({ok, Breakdown}) ->
    BreakdownStrings = [["    - ", Name, ": ", case Quantity of
                                                   F when is_float(F) -> float_to_list(F, [{decimals,0}]); 
                                                   N when is_integer(N) -> integer_to_list(N) 
                                               end] || {Name, Quantity} <- maps:to_list(Breakdown)],
    BreakdownStr = lists:join(", \n", BreakdownStrings),
    BodyStr = io_lib:format("Total Items:~n~s~n", [BreakdownStr]),
    iolist_to_binary(BodyStr).