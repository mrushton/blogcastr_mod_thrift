%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% mod_blogcastr_thrift.erl: ejabberd module that
%% provides a thrift interface
%%
%% Author: Matt Rushton
%% Date: 8/2/09
%% Copyright 2009 Blogcastr, Inc.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(mod_blogcastr_thrift).
-author("Matt Rushton").

-behavior(gen_mod).

-include("ejabberd.hrl").
-include("blogcastr_thrift.hrl").
-include("blogcastr_types.hrl").

-export([start/2, stop/1, handle_function/2, create_user/2, get_user_password/1, create_node/2, delete_node/2, publish_text_item/3, add/2]).

%%thrift functions

%%create a user
create_user(Username, Password) ->
    ?INFO_MSG("Creating user ~s with password ~s", [Username, Password]),
    %%TODO: do not hard code server name
    case ejabberd_auth:try_register(binary_to_list(Username), "blogcastr.com", binary_to_list(Password)) of
        {atomic, ok} ->
            0;
        {atomic, exists} ->
            ?ERROR_MSG("Error creating user ~s: already exists", [Username]),
            1;
        {error, not_allowed} ->
            ?ERROR_MSG("Error creating user ~s: not allowed", [Username]),
            1
    end.

%%get users password
get_user_password(Username) ->
    ?INFO_MSG("Getting user ~s's password", [Username]),
    ejabberd_auth:get_password(binary_to_list(Username), "blogcastr.com").

%%create a pubsub node 
create_node(Username, Node) ->
    ?INFO_MSG("Creating node ~s", [Node]),
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", ""),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
    %%MVR - need to export create_node/7
    %%MVR - alternative would be to simply modify the "default" config in ejabberd
    case mod_pubsub:create_node("pubsub.blogcastr.com", "blogcastr.com", NodeList, Jid, "default", all, [{xmlelement, "x", [{"xmlns", "jabber:x:data"}, {"type", "submit"}], [{xmlelement, "field", [{"var", "FORM_TYPE"}, {"type", "hidden"}], [{xmlelement, "value", [], [{xmlcdata, <<"http://jabber.org/protocol/pubsub#node_config">>}]}]}, {xmlelement, "field", [{"var", "pubsub#notify_retract"}], [{xmlelement, "value", [], [{xmlcdata, <<"0">>}]}]}, {xmlelement, "field", [{"var", "pubsub#persist_items"}], [{xmlelement, "value", [], [{xmlcdata, <<"0">>}]}]}, {xmlelement, "field", [{"var", "pubsub#send_last_published_item"}], [{xmlelement, "value", [], [{xmlcdata, <<"never">>}]}]}]}]) of
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error creating node ~s", [Node]),
            1
    end.

%%TODO: not currently working
%%destroy a pubsub node 
delete_node(Username, Node) ->
    ?INFO_MSG("Deleting node ~s", [Node]),
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", ""),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
    case mod_pubsub:delete_node("pubsub.blogcastr.com", NodeList, Jid) of
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error deleting node ~s", [Node]),
            1
    end.

%%publish text item to a pubsub node 
publish_text_item(Username, Node, Item) ->
    ?INFO_MSG("Publishing text item \"~s\" to node ~s", [Item#textItem.text, Node]),
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", ""),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
    case mod_pubsub:publish_item("pubsub.blogcastr.com", "blogcastr.com", NodeList, Jid, "", [{xmlelement, "event", [{"xmlns", "http://blogcastr.com"}], [{xmlelement, "type", [], [{xmlcdata, <<"postText">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Item#textItem.id))}]}, {xmlelement, "date", [], [{xmlcdata, Item#textItem.date}]}, {xmlelement, "text", [], [{xmlcdata, Item#textItem.text}]}]}]) of
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error publishing text item \"~s\" to node ~s", [Item#textItem.text, Node]),
            1
    end.

%%simple test function
add(X, Y) ->
    ?INFO_MSG("Adding ~B and ~B", [X, Y]),
    X + Y.

%%handle thirft callbacks
handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok ->
            ok;
        Reply ->
            {reply, Reply}
    end.

%%start module
%%TODO: not clear on proper return values
start(_Host, _Opts) ->
    ?INFO_MSG("Starting mod_blogcastr_thrift", []),
    case thrift_server:start_link(9090, blogcastr_thrift, ?MODULE) of
        {ok, Pid} ->
            ?INFO_MSG("Succesfully started thrift server", []),
            put(thrift_server_pid,Pid),
            ok;
        ignore ->
            ?ERROR_MSG("Error starting thrift server", []),
            error;
        {error, Error} ->
            ?ERROR_MSG("Error starting thrift server: ~s", [Error]),
            error
    end.

%%stop module
%%TODO: not clear on proper return values
stop(_Host) ->
    ?INFO_MSG("Stopping mod_blogcastr_thrift", []),
    case thrift_server:stop(get(thrift_server_pid)) of
        ok ->
            ?INFO_MSG("Succesfully stopped thrift server", []),
            ok;
        {error, Reason} ->
            ?ERROR_MSG("Error stopping thrift server: ~s", [Reason]),
            error
    end.
