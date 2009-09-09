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

-export([start/2, stop/1, handle_function/2, create_user/2, destroy_user/1, get_user_password/1, change_user_password/2, create_pubsub_node/2, destroy_pubsub_node/2, subscribe_to_pubsub_node/3, unsubscribe_from_pubsub_node/4, publish_text_post_to_pubsub_node/4, create_muc_room/4, destroy_muc_room/1, get_num_muc_room_occupants/1, send_text_post_to_muc_room/4, send_image_post_to_muc_room/4, send_text_comment_post_to_muc_room/6, send_text_comment_to_muc_occupant/4, add/2]).

-include("ejabberd.hrl").
-include("blogcastr_thrift.hrl").
-include("blogcastr_types.hrl").

-record(muc_online_room, {name_host, pid}).

%%helper functions

user_to_xmlelement(User) ->
    {xmlelement, "user", [], [{xmlelement, "name", [], [{xmlcdata, User#user.name}]}, {xmlelement, "account", [], [{xmlcdata, User#user.account}]}, {xmlelement, "url", [], [{xmlcdata, User#user.url}]}, {xmlelement, "avatar_url", [], [{xmlcdata, User#user.avatar_url}]}]}.

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

%%destroy a user
destroy_user(Username) ->
    ok.

%%get user's password
get_user_password(Username) ->
    ?INFO_MSG("Getting user ~s's password", [Username]),
    ejabberd_auth:get_password(binary_to_list(Username), "blogcastr.com").

%%TODO: change a user's password
change_user_password(Username, Password) ->
    ok.

%%create a pubsub node 
create_pubsub_node(Username, Node) ->
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
destroy_pubsub_node(Username, Node) ->
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

%%subscribe to a pubsub node
%%TODO return subid
subscribe_to_pubsub_node(Username, Resource, Node) ->
    ?INFO_MSG("User ~s@blogcastr.com/~s subscribing to node ~s", [Username, Resource, Node]),
    %%MVR - subscribe_node takes a jid and a jid string
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", binary_to_list(Resource)),
    JidList = binary_to_list(Username) ++ "@blogcastr.com/" ++ binary_to_list(Resource),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
   case mod_pubsub:subscribe_node("pubsub.blogcastr.com", NodeList, Jid, JidList) of 
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error user ~s@blogcastr.com/~s subscribing to node ~s", [Username, Resource, Node]),
            1
   end.

%%unsubscribe from a pubsub node
unsubscribe_from_pubsub_node(Username, Resource, Node, SubId) ->
    ?INFO_MSG("User ~s@blogcastr.com/~s unsubscribing from node ~s", [Username, Resource, Node]),
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", binary_to_list(Resource)),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
    case mod_pubsub:unsubscribe_node("pubsub.blogcastr.com", NodeList, Jid, Jid, "") of 
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error user ~s@blogcastr.com/~s unsubscribing from node ~s", [Username, Resource, Node]),
            1
    end.

%%publish text post to a pubsub node 
publish_text_post_to_pubsub_node(Username, Node, From, Post) ->
    ?INFO_MSG("Publishing text post ~s to node ~s", [Post#textPost.text, Node]),
    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", ""),
    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
    case mod_pubsub:publish_item("pubsub.blogcastr.com", "blogcastr.com", NodeList, Jid, "", [{xmlelement, "event", [{"xmlns", "http://blogcastr.com"}], [{xmlelement, "type", [], [{xmlcdata, <<"postText">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Post#textPost.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, Post#textPost.timestamp}]}, {xmlelement, "text", [], [{xmlcdata, Post#textPost.text}]}]}]) of
        {result, _} ->
            0;
        {error, _} ->
            ?ERROR_MSG("Error publishing text item ~s to node ~s", [Post#textPost.text, Node]),
            1
    end.

%%publish image post to a pubsub node 
%publish_image_post_to_pubsub_node(Username, Node, Post) ->
%    ?INFO_MSG("Publishing image post ~s to node ~s", [Post#imagePost.image_url, Node]),
%    Jid = jlib:make_jid(binary_to_list(Username), "blogcastr.com", ""),
%    NodeList = mod_pubsub:string_to_node(binary_to_list(Node)),
%    case mod_pubsub:publish_item("pubsub.blogcastr.com", "blogcastr.com", NodeList, Jid, "", [{xmlelement, "event", [{"xmlns", "http://blogcastr.com"}], [{xmlelement, "type", [], [{xmlcdata, <<"postImage">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Post#imagePost.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, Post#imagePost.timestamp}]}, {xmlelement, "image_url", [], [{xmlcdata, Post#imagePost.image_url}]}]}]) of
%        {result, _} ->
%            0;
%        {error, _} ->
%            ?ERROR_MSG("Error publishing image post ~s to node ~s", [Post#imagePost.image_url, Node]),
%            1
%    end.

%%%TODO: why is room visibible in discovery and not after restart 
%%create a muc room with the given title and subject
create_muc_room(Username, Room, Title, Subject) ->
    ?INFO_MSG("Creating muc room ~s", [Room]),
    %%store room
    Opts = [{title, binary_to_list(Title)}, {allow_change_subj, false}, {allow_query_users, false}, {allow_private_messages, true}, {allow_visitor_status, false}, {allow_visitor_nickchange, false}, {public, false}, {public_list, false}, {persistent, true}, {moderated, true}, {members_by_default, true}, {members_only, false}, {allow_user_invites, false}, {password_protected, false}, {captcha_protected, false}, {password, []}, {anonymous, false}, {logging, false}, {max_users, 200}, {affiliations, [{{binary_to_list(Username), "blogcastr.com", []}, owner}]}, {subject, binary_to_list(Subject)}, {subject_author, binary_to_list(Username)}],
    mod_muc:store_room("conference.blogcastr.com", binary_to_list(Room), Opts),
    %%start room
    case mod_muc_room:start("conference.blogcastr.com", "blogcastr.com", {muc, muc, muc_admin, muc}, binary_to_list(Room), 0, none, Opts) of
        {ok, Pid} ->
            F = fun() ->
                mnesia:write(#muc_online_room{name_host = {Room, "conference.blogcastr.com"}, pid = Pid})
            end,
            mnesia:transaction(F),
            0;
        _ ->
            ?INFO_MSG("Error creating muc room ~s", [Room]),
            1
    end.

destroy_muc_room(Room) ->
    ok.

get_num_muc_room_occupants(Room) ->
    ok.

%%send text post to a muc room
%%TODO: currently need to pass resource of a logged in user, just using dashboard for now
send_text_post_to_muc_room(Username, Room, From, Post) ->
    ?INFO_MSG("Sending text post to muc room ~s from ~s", [Room, Username]),
    UsernameList = binary_to_list(Username),
    RoomList = binary_to_list(Room),
    ToJid = jlib:make_jid(RoomList, "conference.blogcastr.com", ""),
    FromJid = jlib:make_jid(UsernameList, "blogcastr.com", "dashboard"),
    %%could also route using mod_muc_room:route/4 which would be faster
    ejabberd_router:route(FromJid, ToJid, {xmlelement, "message", [{"to", RoomList ++ "@conference.blogcastr.com"}, {"from", UsernameList ++ "@blogcastr.com/dashboard"}, {"type", "groupchat"}], [{xmlelement, "body", [], [{xmlelement, "type", [], [{xmlcdata, <<"textPost">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Post#textPost.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, list_to_binary(integer_to_list(Post#textPost.timestamp))}]}, {xmlelement, "medium", [], [{xmlcdata, Post#textPost.medium}]}, {xmlelement, "text", [], [{xmlcdata, Post#textPost.text}]}, {xmlelement, "from", [], [user_to_xmlelement(From)]}]}]}), 
    0.

%%send image post to a muc room
%%TODO: currently need to pass resource of a logged in user, just using dashboard for now
send_image_post_to_muc_room(Username, Room, From, Post) ->
    ?INFO_MSG("Sending image post to muc room ~s from ~s", [Room, Username]),
    UsernameList = binary_to_list(Username),
    RoomList = binary_to_list(Room),
    ToJid = jlib:make_jid(RoomList, "conference.blogcastr.com", ""),
    FromJid = jlib:make_jid(UsernameList, "blogcastr.com", "dashboard"),
    %%could also route using mod_muc_room:route/4 which would be faster
    ejabberd_router:route(FromJid, ToJid, {xmlelement, "message", [{"to", RoomList ++ "@conference.blogcastr.com"}, {"from", UsernameList ++ "@blogcastr.com/dashboard"}, {"type", "groupchat"}], [{xmlelement, "body", [], [{xmlelement, "type", [], [{xmlcdata, <<"imagePost">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Post#imagePost.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, list_to_binary(integer_to_list(Post#imagePost.timestamp))}]}, {xmlelement, "medium", [], [{xmlcdata, Post#imagePost.medium}]}, {xmlelement, "image_url", [], [{xmlcdata, Post#imagePost.image_url}]}, {xmlelement, "from", [], [user_to_xmlelement(From)]}]}]}), 
    0.

%%send text comment post to a muc room
%%TODO: currently need to pass resource of a logged in user, just using dashboard for now
send_text_comment_post_to_muc_room(Username, Room, PostFrom, Post, CommentFrom, Comment) ->
    ?INFO_MSG("Sending text comment post to muc room ~s from ~s", [Room, Username]),
    UsernameList = binary_to_list(Username),
    RoomList = binary_to_list(Room),
    ToJid = jlib:make_jid(RoomList, "conference.blogcastr.com", ""),
    FromJid = jlib:make_jid(UsernameList, "blogcastr.com", "dashboard"),
    %%could also route using mod_muc_room:route/4 which would be faster
    ejabberd_router:route(FromJid, ToJid, {xmlelement, "message", [{"to", RoomList ++ "@conference.blogcastr.com"}, {"from", UsernameList ++ "@blogcastr.com/dashboard"}, {"type", "groupchat"}], [{xmlelement, "body", [], [{xmlelement, "type", [], [{xmlcdata, <<"commentPost">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Post#commentPost.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, list_to_binary(integer_to_list(Post#commentPost.timestamp))}]}, {xmlelement, "medium", [], [{xmlcdata, Post#commentPost.medium}]}, {xmlelement, "from", [], [user_to_xmlelement(PostFrom)]}, {xmlelement, "comment", [], [{xmlelement, "type", [], [{xmlcdata, <<"textComment">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Comment#textComment.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, list_to_binary(integer_to_list(Comment#textComment.timestamp))}]}, {xmlelement, "text", [], [{xmlcdata, Comment#textComment.text}]}, {xmlelement, "medium", [], [{xmlcdata, Comment#textComment.medium}]}, {xmlelement, "from", [], [user_to_xmlelement(CommentFrom)]}]}]}]}), 
    0.

%%send text comment post to a muc occupant
send_text_comment_to_muc_occupant(To, From_Jid, From, Comment) ->
    ?INFO_MSG("Sending text comment to ~s from ~s", [To, From]),
    ToJid = jlib:string_to_jid(binary_to_list(To)),
    FromJid = jlib:string_to_jid(binary_to_list(From_Jid)),
    %%TODO - better error checking
    ejabberd_router:route(FromJid, ToJid, {xmlelement, "message", [{"to", To}, {"from", From}, {"type", "chat"}], [{xmlelement, "body", [], [{xmlelement, "type", [], [{xmlcdata, <<"textComment">>}]}, {xmlelement, "id", [], [{xmlcdata, list_to_binary(integer_to_list(Comment#textComment.id))}]}, {xmlelement, "timestamp", [], [{xmlcdata, list_to_binary(integer_to_list(Comment#textComment.timestamp))}]}, {xmlelement, "text", [], [{xmlcdata, Comment#textComment.text}]}, {xmlelement, "from", [], [{xmlelement, "user", [], [{xmlelement, "name", [], [{xmlcdata, From#user.name}]}, {xmlelement, "account", [], [{xmlcdata, From#user.account}]}, {xmlelement, "url", [], [{xmlcdata, From#user.url}]}, {xmlelement, "avatar_url", [], [{xmlcdata, From#user.avatar_url}]}]}, {xmlelement, "medium", [], [{ xmlcdata, Comment#textComment.medium}]}]}]}]}),
    0.

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
