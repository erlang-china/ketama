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

-module(ketama_misc).

-export([get_hash_value/1, get_node_hash_value/2]).
-export([is_tab_exist/1]).
-export([to_list/1]).

get_hash_value(Key)->
    <<HashVal:32/big, _/binary>>  = erlang:md5(Key),
    HashVal.


get_node_hash_value(Key, true)->
    <<HashVal_1:32/big, 
      HashVal_2:32/big,
      HashVal_3:32/big,
      HashVal_4:32/big>>  = erlang:md5(Key),
    [HashVal_1, HashVal_2, HashVal_3, HashVal_4];

get_node_hash_value(Key, false)->
    <<HashVal:32/big, _/binary>>  = erlang:md5(Key),
    [HashVal].



is_tab_exist(undefined) -> false;
is_tab_exist(Tab) when is_atom(Tab)->
    TabName = Tab,
    TabName =:= ets:info(Tab, name).


to_list(Val) when is_integer(Val)-> integer_to_list(Val);
to_list(Val) when is_atom(Val)-> atom_to_list(Val);
to_list(Val) -> Val.