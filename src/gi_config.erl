%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2020 10:43 PM
%%%-------------------------------------------------------------------
-module(gi_config).
-author("artman41").

%% API
-export([
    get_cowboy_port/0,
    get_cache_time/0,
    get_recipe_mappings/0,
    get_recipe_mapping/1
]).

-type recipe_mapping() :: {Requirements :: list(), OutputQuantity :: non_neg_integer()}.

get_cowboy_port() ->
    get_env(cowboy_port, 80).

get_cache_time() ->
    get_env(cache_time, 60 * 60 * 1000).

-spec get_recipe_mappings() -> #{Name :: binary() => recipe_mapping()}.
get_recipe_mappings() ->
    get_env(recipe_mappings, #{}).

-spec get_recipe_mapping(Name :: binary()) -> undefined | recipe_mapping().
get_recipe_mapping(Name) when is_binary(Name) ->
    get_recipe_mapping_(gi_utils:bin_to_lower(Name), maps:to_list(get_recipe_mappings())).

%% Internal

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    application:get_env(get_items, Key, Default).
    
-spec get_recipe_mapping_(Name :: binary(), list({Key :: binary(), Value :: recipe_mapping()})) -> undefined | recipe_mapping().
get_recipe_mapping_(_Name, []) ->
    undefined;
get_recipe_mapping_(Name, [{Key, Value} | Tail]) ->
    case gi_utils:bin_to_lower(Key) of
        Name ->
            Value;
        _ ->
            get_recipe_mapping_(Name, Tail)
    end.