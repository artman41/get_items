%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2020 11:18 PM
%%%-------------------------------------------------------------------
-module(gi_utils).
-author("artman41").

%% API
-export([
    bin_to_lower/1
]).

bin_to_lower(Bin) when is_binary(Bin) ->
    bin_to_lower_(Bin, <<>>).
    
bin_to_lower_(<<>>, Acc0) ->
    Acc0;
bin_to_lower_(<<H, Tail/binary>>, Acc0) ->
    Acc1 = <<Acc0/binary, (string:to_lower(H))>>,
    bin_to_lower_(Tail, Acc1).