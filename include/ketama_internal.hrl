-ifndef(KETAMA_INTERNAL_H).
-define(KETAMA_INTERNAL_H, true).

-define(TAB_NODES,            ets_ketama_nodes_).
-define(STR_TAB_NODES,        "ets_ketama_nodes_").
-define(TAB_STASHES,          ets_ketama_stashes).
-define(TAB_NODE_PROPERTY,    ets_ketama_node_property_).
-define(TAB_RINGS,            ets_ketama_rings).

-define(RING_TAB(RING_NAME), 
            list_to_atom(lists:concat([?TAB_NODES, (RING_NAME)]))).

-define(NODE_TAB(RING_NAME), 
            list_to_atom(lists:concat([?TAB_NODE_PROPERTY, (RING_NAME)]))).

-record(node_item, { id        :: integer(),
                     copy_id   :: integer(), 
                     hash      :: integer(), 
                     object    :: any()}).  

-define(MISC, ketama_misc).

-endif.