%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(get_items_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

cowboy_spec() ->
    Dispatch =
        cowboy_router:compile([
            {'_', [
                {"/css/[...]", cowboy_static, {priv_dir, get_items, "css"}},
                {"/js/[...]", cowboy_static, {priv_dir, get_items, "js"}},
                
                {"/get_items", get_items_endpoint, []},
                
                {"/[...]", cowboy_static, {priv_file, get_items, "html/index.html"}}
            ]}
        ]),
    #{
        id => 'get_items_webserver',
        start => {cowboy, start_clear, [
            http, [{port, gi_config:get_cowboy_port()}], #{env => #{dispatch => Dispatch}}
        ]},
        restart => permanent,
        shutdown => 2000,
        type => worker
    }.

init([]) ->
    Children = [
        #{
            id => 'get_items',
            start => {get_items_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        cowboy_spec()
    ],
    
    Strategy = #{
        strategy => one_for_one,
        intensity => 5,
        period => 30
    },
    {ok, {Strategy, Children}}.
