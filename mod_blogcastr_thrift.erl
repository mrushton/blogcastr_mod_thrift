%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mod_blogcastr_thrift.erl: ejabberd module that
% provides a thrift interface
%
% Author: Matt Rushton
% Date: 8/2/09
% Copyright 2009 Blogcastr, Inc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mod_blogcastr_thrift).
-author("Matt Rushton").

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("blogcastr_thrift.hrl").
-include("blogcastr_types.hrl").

-export([start/2, stop/1, handle_function/2, add/2]).

%thrift functions
add(X, Y) ->
    ?INFO_MSG("Adding X+Y", []),
    X+Y.

%handle thirft callbacks
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    ?INFO_MSG("Handling Function", []),
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

%start module
start(_Host, _Opts) ->
    ?INFO_MSG("Starting mod_blogcastr_thrift", []),
    thrift_server:start_link(9090, blogcastr_thrift, ?MODULE),
    ok.

%stop module
stop(_Host) ->
    ?INFO_MSG("Stopping mod_blogcastr_thrift", []),
    thrift_server:stop(""),
    ok.
