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

-module(ketama_ring).

-include("ketama.hrl").
-include("ketama_internal.hrl").


-export([list_rings/0, 
         add_ring/1, 
         remove_ring/1, 
         is_ring_exist/1,
         get_ring_summary/1,
         balance_test/2]).

-export([init_nodes/1,
         get_node/2,
         list_nodes/1,
         add_node/2, 
         remove_node/2, 
         clear_nodes/1, 
         set_node_weight/3,
         set_node_copies/3]).

-export([get_object/2]).

list_rings() ->
    Items = ets:match(?TAB_RINGS, '$1'),
    lists:flatten(Items).

get_object(Ring, Key)->
    case get_ring_opts(Ring) of
        {ok, #ring_opt{ match_operator  = Op }} ->
            Hash0   = ?MISC:get_hash_value(Key),
            RingTab = ?RING_TAB(Ring),
            Hash =
            case Op of
                '>=' ->
                    Hash0-1;
                '>' ->
                    Hash0
            end,
            case ets:next(RingTab, Hash) of
                '$end_of_table' -> 
                    case ets:first(RingTab) of 
                        '$end_of_table' ->
                            {error, get_object_failed};
                        ItemKey->
                            get_object_by_hash(RingTab, ItemKey)
                    end;
                ItemKey->
                    get_object_by_hash(RingTab, ItemKey)
            end;
        Error ->
            Error
    end.

add_ring(#ring_opt{ name = Ring} = RingOpt) ->
    case is_ring_exist(Ring) of 
        false ->
            RingTab  = ?RING_TAB(Ring),
            RingTab  = ets_mgr:soft_new(RingTab,[ named_table,
                                                  protected,
                                                  ordered_set,
                                                  {keypos, #node_item.hash},
                                                  {write_concurrency, false}, 
                                                  {read_concurrency,  true}]),

            NodeTab  = ?NODE_TAB(Ring),
            NodeTab  = ets_mgr:soft_new(NodeTab,[ named_table,
                                                  protected,
                                                  {keypos, #node.id},
                                                  {write_concurrency, false}, 
                                                  {read_concurrency,  true}]),
            true = ets:insert(?TAB_RINGS, RingOpt),
            ok;
        true ->
            {error, ring_already_exist}
    end.

remove_ring(Ring) ->
    RingTab = ?RING_TAB(Ring),
    case ?MISC:is_tab_exist(RingTab) of 
         true ->
             true    = ets:delete(RingTab),
             NodeTab = ?NODE_TAB(Ring),
             true    = ets:delete(NodeTab),
             ok;
         false ->
            {error, ring_not_exist}
    end.

is_ring_exist(Ring) ->
    ets:member(?TAB_RINGS, Ring).

get_ring_summary(Ring) ->
    case list_nodes(Ring) of 
        {ok, Nodes} ->
            ItemCounts    =
            [ {Id, count_node_items(Ring, Id)}
              ||#node{id = Id} <- Nodes],
            
            TotalCopies = lists:sum([Number|| {_Id, Number} <- ItemCounts]),
            
            [begin
                ItemCount  = proplists:get_value(Id, ItemCounts),
                Percentage =
                case ItemCount > 0 of
                    true ->
                        ItemCount/TotalCopies;
                    false ->
                        0
                end,
                #node_summary{id         = Id, 
                              weight     = Weight,
                              percentage = Percentage,
                              copies_num = ItemCount}
             end
             ||#node{id = Id, weight = Weight} <- Nodes];
        Error ->
            Error
    end.

get_ring_opts(RingName) ->
    case ets:lookup(?TAB_RINGS, RingName) of
        [Obj] ->
            {ok, Obj};
        _->
            {error, ring_not_exist}
    end.


get_node(Ring, NodeId) ->
    NodeTab = ?NODE_TAB(Ring),
    case ?MISC:is_tab_exist(NodeTab) of 
        true ->
            case ets:lookup(NodeTab, NodeId) of
                [Node]->
                    {ok, Node};
                _->
                    {error, node_not_exist}
            end;
        false ->
            {error, ring_not_exist}
    end.

list_nodes(Ring) ->
    NodeTab = ?NODE_TAB(Ring),
    case ?MISC:is_tab_exist(NodeTab) of 
        true ->
            {ok, lists:flatten(ets:match(NodeTab, '$1'))};
        false ->
            {error, ring_not_exist}
    end.

add_node(Ring, #node{ id = NodeId} = Node) ->
    RingTab = ?RING_TAB(Ring),
    case ?MISC:is_tab_exist(RingTab) of 
         true ->
            case get_node(Ring, NodeId) of 
                {error, node_not_exist} ->
                    NodeTab = ?NODE_TAB(Ring),
                    true    = ets:insert(NodeTab, Node),
                    init_nodes(Ring);
                {ok, _Node} ->
                    {error, node_alrady_exist};
                Error ->
                    Error
            end;
         false ->
            {error, ring_not_exist}
    end.

remove_node(Ring, NodeId)->
    RingTab = ?RING_TAB(Ring),
    case ?MISC:is_tab_exist(RingTab) of 
         true ->
            CopiesMatchSpec = node_match_spec(NodeId),
            true    = ets:match_delete(RingTab, CopiesMatchSpec),
            NodeTab = ?NODE_TAB(Ring),
            true    = ets:delete(NodeTab, NodeId),
            ok;
         false ->
            {error, ring_not_exist}
    end.

clear_nodes(Ring)->
    RingTab = ?RING_TAB(Ring),
    case ?MISC:is_tab_exist(RingTab) of 
         true ->
             true = ets:delete_all_objects(RingTab),
             NodeTab = ?NODE_TAB(Ring),
             true = ets:delete_all_objects(NodeTab),
             ok;
         false ->
            {error, ring_not_exist}
    end.

set_node_weight(Ring, NodeId, NewWeight) ->
    case get_ring_opts(Ring) of
        {ok, #ring_opt{copies_gen_type = GenType}} ->
            case GenType of
                weight ->
                    case get_node(Ring, NodeId) of 
                        {ok, #node{weight = Weight} = Node} ->
                            case NewWeight == Weight of
                                false ->
                                    NewNode = Node#node{weight = NewWeight},
                                    true    = ets:insert(?NODE_TAB(Ring), 
                                                          NewNode),
                                    init_nodes(Ring);
                                true ->
                                    {error, weight_nochanges}
                            end;
                        Error ->
                            Error
                    end;
                specific->
                    {error, copies_gen_type_is_specific}
            end;
        Error ->
            Error
    end.

set_node_copies(Ring, NodeId, NewNum) ->
    case get_ring_opts(Ring) of
        {ok, #ring_opt{ copies_gen_type = GenType, 
                        concat_char     = ConcatChar,
                        expand_node     = ExpandNode}} ->
            case GenType of 
                specific ->
                    case get_node(Ring, NodeId) of 
                        {ok, #node{ copies_num = Number, 
                                    hash_seed  = HashSeed,
                                    object     = Object} = Node} ->
                            case NewNum == Number of
                                false ->
                                    RingTab = ?RING_TAB(Ring),
                                    CopiesMatchSpec = node_match_spec(NodeId),
                                    true    = ets:match_delete(RingTab, 
                                                               CopiesMatchSpec),
                                    Copies  = make_nodes(NewNum, 
                                                         NodeId, 
                                                         HashSeed, 
                                                         Object, 
                                                         ExpandNode,
                                                         ConcatChar, 
                                                         []),
                                    true    = ets:insert(RingTab, Copies),
                                    NewNode = Node#node{copies_num = NewNum},
                                    true = ets:insert(?NODE_TAB(Ring), NewNode),
                                    ok;
                                true ->
                                    {error, copies_num_nochanges}
                            end;
                        Error ->
                            Error
                    end;
                weight ->
                    {error, copies_gen_type_is_weight}
            end;
        Error ->
            Error
    end.



init_nodes(Ring) ->
    case list_nodes(Ring) of 
        {ok, Nodes} ->
            {ok, #ring_opt{ node_copies     = NodeCopies, 
                            expand_node     = ExpandNode,
                            concat_char     = ConcatChar,
                            copies_gen_type = GenType}} = get_ring_opts(Ring),
            case GenType of 
                weight ->
                    clear_node_copies(Ring),
                    RingTab       = ?RING_TAB(Ring),
                    TotalWeight   = lists:sum([W ||#node{ weight = W }  <- Nodes]),
                    [begin
                        CopiesNum  = trunc(NodeCopies * Weight / TotalWeight),
                        ItemNodes = make_nodes(CopiesNum, NodeId, HashSeed, Object, ExpandNode, ConcatChar, []),
                        true      = ets:insert(RingTab, ItemNodes)
                     end
                     || #node{ id        = NodeId, 
                               hash_seed = HashSeed, 
                               object    = Object,
                               weight    = Weight } <- Nodes],
                    ok;
                specific ->
                    clear_node_copies(Ring),
                    RingTab       = ?RING_TAB(Ring),
                    [begin
                        ItemNodes = make_nodes(CopiesNum, NodeId, HashSeed, Object, ExpandNode, ConcatChar, []),
                        true      = ets:insert(RingTab, ItemNodes)
                     end
                     || #node{ id        = NodeId, 
                               hash_seed = HashSeed, 
                               object    = Object,
                               copies_num = CopiesNum } <- Nodes],
                    ok;
                Error->
                    Error
            end;
        Error ->
            Error
    end.

clear_node_copies(Ring) -> 
    RingTab       = ?RING_TAB(Ring),
    NodeMatchSpec = node_match_spec('_'),
    true          = ets:match_delete(RingTab, NodeMatchSpec),
    ok.

balance_test(_Ring, KeyNum) when KeyNum =<0 -> 
  {error, key_number_should_greater_than_zero};
balance_test(Ring, KeyNum) ->
    Ret = 
    [ begin 
        {ok, {NodeId, _Object}} = get_object(Ring, integer_to_list(Key)),
        NodeId
      end
    || Key <- lists:seq(1, KeyNum)],
    CountList = list_key_count(Ret, dict:new()),
    {ok, [{Id, Number, Number/KeyNum}||{Id, Number}<- CountList]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_object_by_hash(RingTab, Hash) -> 
    case ets:lookup(RingTab, Hash) of 
        [#node_item{ id     = NodeId, 
                     object = Object}] ->
                {ok, {NodeId, Object}};
        _->
            {error, get_object_failed}
    end.


list_key_count([], Dict) -> dict:to_list(Dict);
list_key_count([Key|T], Dict) -> 
    NewDict = dict:update_counter(Key, 1, Dict),
    list_key_count(T, NewDict).

node_match_spec(NodeId) ->
   #node_item{ id        = NodeId,
               copy_id   = '_',
               hash      = '_',
               object    = '_'}.

make_nodes(Number, NodeId, HashSeed, Object, Expand, ConcatChar, Nodes) when Number > 0 ->
    NewNodes = make_node_items(NodeId, Number, HashSeed, Object, Expand, ConcatChar),
    make_nodes(Number-1, NodeId, HashSeed, Object, Expand, ConcatChar, [NewNodes|Nodes]);
make_nodes(_Number, _NodeId, _HashSeed, _Object, _Expand, _ConcatChar, Nodes) -> 
    lists:flatten(Nodes).

make_node_items(NodeId, CopyId, HashSeed, Object, Expand, ConcatChar) ->
    HashSeed0 = io_lib:format("~s~s~b",[HashSeed, ConcatChar, CopyId]),
    [#node_item{ id         = NodeId, 
                 copy_id    = CopyId,
                 hash       = Hash,
                 object     = Object}  
     ||Hash <- ?MISC:get_node_hash_value(HashSeed0, Expand)].

count_node_items(Ring, NodeId) ->
    RingTab = ?RING_TAB(Ring),

    ets:select_count(RingTab, [{#node_item{ id        = '$1',
                                            copy_id   = '_',
                                            hash      = '_',
                                            object    = '_'},
                                [{'==', '$1', NodeId}],
                                [true]}]).
