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

-module(ketama_stash).

-include("ketama.hrl").
-include("ketama_internal.hrl").

-export([stash/2, 
         stash_apply/2, 
         get_stash/2, 
         list_stashes/1, 
         remove_stash/2,
         remove_stashes/1, 
         clear_stashes/0,
         is_stash_exist/2]).

stash(Ring, Name) ->
    case ?MISC:is_tab_exist(?TAB_STASHES) of 
        true ->
            RingTab = ?RING_TAB(Ring),
            case ?MISC:is_tab_exist(RingTab) of 
                 true ->
                    case is_stash_exist(Ring, Name) of 
                        true->
                            {error, stash_already_exist};
                        false->
                            case ketama_ring:list_nodes(Ring) of 
                                {ok, Nodes} ->
                                    true  = ets:insert(?TAB_STASHES, 
                                    #stash{ id    = get_stash_id(Ring, Name), 
                                            ring  = Ring, 
                                            name  = Name, 
                                            nodes = Nodes}),
                                    ok;
                                Error->
                                    Error
                            end
                    end;
            false ->
                    {error, ring_not_exist}
            end;
        false ->
            {error, stash_storge_crashed}
    end.

stash_apply(Ring, Name) ->
    case ?MISC:is_tab_exist(?TAB_STASHES) of 
        true ->
            StashId = get_stash_id(Ring, Name),
            case ets:lookup(?TAB_STASHES, StashId) of 
                [#stash{nodes = Nodes}] ->
                    RingTab = ?RING_TAB(Ring),
                    case ?MISC:is_tab_exist(RingTab) of 
                        true ->
                            ok;
                        false->
                            ketama_ring:add_ring(Ring)
                    end,
                    case ketama_ring:clear_nodes(Ring) of 
                        ok ->
                            true = ets:insert(?NODE_TAB(Ring), Nodes),
                            ketama_ring:init_nodes(Ring);
                        ClearErr ->
                            ClearErr
                    end;
                _->
                    {error, stash_not_exist}
            end;
        false ->
            {error, stash_storge_crashed}
    end.

remove_stash(Ring, Name)->
    case ?MISC:is_tab_exist(?TAB_STASHES) of 
        true ->
            StashId = get_stash_id(Ring, Name),
            true    = ets:delete(?TAB_STASHES, StashId),
            ok;
        false ->
            {error, stash_storge_crashed}
    end.

remove_stashes(Ring)->
    case ?MISC:is_tab_exist(?TAB_STASHES) of 
        true ->
            true = ets:match_delete(?TAB_STASHES, #stash{ id    = '_', 
                                                          ring  = Ring, 
                                                          name  = '_', 
                                                          nodes = '_', 
                                                          time  = '_'});
        false ->
            {error, stash_storge_crashed}
    end.

clear_stashes()->
    case ?MISC:is_tab_exist(?TAB_STASHES) of 
        true ->
            true = ets:match_delete(?TAB_STASHES, '$1'),
            ok;
        false ->
            {error, stash_storge_crashed}
    end.

get_stash(Ring, Name) ->
    case ?MISC:is_tab_exist(?TAB_STASHES) of
        true ->
            StashId = get_stash_id(Ring, Name),
            MatchSpec = #stash{ id    = StashId, 
                                ring  = '_', 
                                name  = '_', 
                                nodes = '_', 
                                time  = '_'},
            lists:flatten(ets:match_object(?TAB_STASHES, MatchSpec));
        false ->
            {error, stash_storge_crashed}
    end.

list_stashes(Ring) ->
    case ?MISC:is_tab_exist(?TAB_STASHES) of
        true ->
            MatchSpec = #stash{ id    = '_', 
                                ring  = Ring, 
                                name  = '_', 
                                nodes = '_', 
                                time  = '_'},
            lists:flatten(ets:match_object(?TAB_STASHES, MatchSpec));
        false ->
            {error, stash_storge_crashed}
    end.

get_stash_id(Ring, Name) ->
    lists:concat([Ring, "_" , Name]).

is_stash_exist(Ring, Name)->
    StashId = get_stash_id(Ring, Name),
    ets:member(?TAB_STASHES, StashId).