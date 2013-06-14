%% -------------------------------------------------------------------
%% Copyright (c) 2013 Xujin Zheng (zhengxujin@adsage.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% -------------------------------------------------------------------

-module(ketama).

-include("ketama.hrl").
-include("ketama_internal.hrl").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-export([list_rings/0, 
         add_ring/1, 
         remove_ring/1, 
         is_ring_exist/1,
         get_ring_summary/1,
         balance_test/2]).

-export([add_node/2, 
         get_node/2,
         list_nodes/1,
         remove_node/2, 
         clear_nodes/1, 
         set_node_weight/3,
         set_node_copies/3]).

-export([get_object/2]).

-export([stash/2, 
         stash_apply/2, 
         get_stash/2, 
         list_stashes/0,
         list_stashes/1, 
         remove_stash/2,
         remove_stashes/1, 
         clear_stashes/0,
         is_stash_exist/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-export([start/0, stop/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    ensure_started(ets_mgr),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

init([]) ->
    ?TAB_STASHES = ets_mgr:soft_new(?TAB_STASHES,[ named_table,
                                                   protected,
                                                   {keypos, #stash.id},
                                                   {write_concurrency, false}, 
                                                   {read_concurrency,  true}]),
    ?TAB_RINGS   = ets_mgr:soft_new(?TAB_RINGS,[ named_table,
                                                 protected,
                                                 {keypos, #ring_opt.name},
                                                 {write_concurrency, false}, 
                                                 {read_concurrency,  true}]),
    {ok, #state{}}.

list_rings() ->
    ketama_ring:list_rings().

add_ring(Ring) ->
    gen_server:call(?SERVER, {add_ring, Ring}).

remove_ring(Ring) when is_atom(Ring) ->
    gen_server:call(?SERVER, {remove_ring, Ring}).

is_ring_exist(Ring) ->
    ketama_ring:is_ring_exist(Ring).

get_ring_summary(Ring) ->
    ketama_ring:get_ring_summary(Ring).

balance_test(Ring, KeyNum) ->
    ketama_ring:balance_test(Ring, KeyNum).

add_node(Ring, Node) ->
    gen_server:call(?SERVER, {add_node, Ring, Node}).

get_node(Ring, NodeId) ->
    ketama_ring:get_node(Ring, NodeId).

list_nodes(Ring) ->
    ketama_ring:list_nodes(Ring).

remove_node(Ring, NodeId)->
    gen_server:call(?SERVER, {remove_node, Ring, NodeId}).

clear_nodes(Ring)->
    gen_server:call(?SERVER, {clear_nodes, Ring}).

set_node_weight(Ring, NodeId, Weight) ->
    gen_server:call(?SERVER, {set_node_weight, Ring, NodeId, Weight}).

set_node_copies(Ring, NodeId, NewNumber) ->
    gen_server:call(?SERVER, {set_node_copies, Ring, NodeId, NewNumber}).



get_object(Ring, Key)->
    ketama_ring:get_object(Ring, Key).


    
stash(Ring, Name) ->
    gen_server:call(?SERVER, {stash, Ring, Name}).

stash_apply(Ring, Name) ->
    gen_server:call(?SERVER, {stash_apply, Ring, Name}).

get_stash(Ring, Name)->
    ketama_stash:get_stash(Ring, Name).

list_stashes()->
    ketama_stash:list_stashes('$1').

list_stashes(Ring)->
    ketama_stash:list_stashes(Ring).

remove_stash(Ring, Name)->
    gen_server:call(?SERVER, {remove_stash, Ring, Name}).

remove_stashes(Ring)->
    gen_server:call(?SERVER, {remove_stashes, Ring}).

clear_stashes()->
    gen_server:call(?SERVER, {clear_stashes}).

is_stash_exist(Ring, Name)->
    ketama_stash:is_stash_exist(Ring, Name).



handle_call({add_ring, Ring}, _From, State) when is_record(Ring, ring_opt)->
    Reply = ketama_ring:add_ring(Ring),
    {reply, Reply, State};
handle_call({list_rings}, _From, State)->
    Reply = ketama_ring:list_rings(),
    {reply, Reply, State};
handle_call({remove_ring, Ring}, _From, State) when is_atom(Ring)->
    Reply = ketama_ring:remove_ring(Ring),
    {reply, Reply, State};
handle_call({is_ring_exist, Ring}, _From, State) when is_atom(Ring)->
    Reply = ketama_ring:is_ring_exist(Ring),
    {reply, Reply, State};
handle_call({get_ring_summary, Ring}, _From, State) when is_atom(Ring)->
    Reply = ketama_ring:get_ring_summary(Ring),
    {reply, Reply, State};
handle_call({balance_test, Ring, KeyNum}, _From, State) 
                                                when is_atom(Ring),
                                                     is_integer(KeyNum)->
    Reply = ketama_ring:balance_test(Ring, KeyNum),
    {reply, Reply, State};

handle_call({add_node, Ring, Node}, _From, State) 
                                    when is_atom(Ring), 
                                         is_record(Node, node)->
    Reply = ketama_ring:add_node(Ring, Node),
    {reply, Reply, State};
handle_call({get_node, Ring, NodeId}, _From, State) 
                                    when is_atom(Ring), 
                                         is_integer(NodeId)->
    Reply = ketama_ring:get_node(Ring, NodeId),
    {reply, Reply, State};
handle_call({list_nodes, Ring}, _From, State) when is_atom(Ring) ->
    Reply = ketama_ring:list_nodes(Ring),
    {reply, Reply, State};
handle_call({remove_node, Ring, NodeId}, _From, State) 
                                when is_atom(Ring),
                                     is_integer(NodeId)->
    Reply = ketama_ring:remove_node(Ring, NodeId),
    {reply, Reply, State};
handle_call({clear_nodes, Ring}, _From, State) when is_atom(Ring)->
    Reply = ketama_ring:clear_nodes(Ring),
    {reply, Reply, State};
handle_call({set_node_weight, Ring, NodeId, Weight}, _From, State) 
                                when Weight >= 0, 
                                     is_integer(NodeId),
                                     is_atom(Ring)->
    Reply = ketama_ring:set_node_weight(Ring, NodeId, Weight),
    {reply, Reply, State};
handle_call({set_node_copies, Ring, NodeId, NewNumber}, _From, State) 
                                when NewNumber >= 0, 
                                     is_integer(NodeId),
                                     is_atom(Ring)->
    Reply = ketama_ring:set_node_copies(Ring, NodeId, NewNumber),
    {reply, Reply, State};



handle_call({stash, Ring, Name}, _From, State) 
                            when is_atom(Ring), 
                                 is_atom(Name)->
    Reply = ketama_stash:stash(Ring, Name),
    {reply, Reply, State};
handle_call({stash_apply, Ring, Name}, _From, State) 
                                    when is_atom(Ring), 
                                         is_atom(Name)->
    Reply = ketama_stash:stash_apply(Ring, Name),
    {reply, Reply, State};
handle_call({get_stash, Ring, Name}, _From, State) 
                                    when is_atom(Ring), 
                                         is_atom(Name)->
    Reply = ketama_stash:get_stash(Ring, Name),
    {reply, Reply, State};
handle_call({list_stashes}, _From, State) ->
    Reply = ketama_stash:list_stashes(),
    {reply, Reply, State};
handle_call({list_stashes, Ring}, _From, State) 
                                    when is_atom(Ring)->
    Reply = ketama_stash:list_stashes(Ring),
    {reply, Reply, State};
handle_call({remove_stash, Ring, Name}, _From, State) 
                                    when is_atom(Ring), 
                                         is_atom(Name)->
    Reply = ketama_stash:remove_stash(Ring, Name),
    {reply, Reply, State};
handle_call({remove_stashes, Ring}, _From, State) when is_atom(Ring)->
    Reply = ketama_stash:remove_stashes(Ring),
    {reply, Reply, State};
handle_call({clear_stashes}, _From, State) ->
    Reply = ketama_stash:clear_stashes(),
    {reply, Reply, State};
handle_call({is_stash_exist, Ring, Name}, _From, State)
                                        when is_atom(Ring), 
                                             is_atom(Name)->
    Reply = ketama_stash:is_stash_exist(Ring, Name),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
