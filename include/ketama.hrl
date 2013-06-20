-ifndef(KETAMA_H).
-define(KETAMA_H, true).

-record(node, {id             :: integer(), 
               hash_seed      :: string(),
               weight     = 0 :: integer(),
               copies_num = 0 :: integer(),
               object         :: any()}).

-record(stash, { id    :: atom(),
                 ring  :: atom(), 
                 name  :: atom(), 
                 nodes :: list(),
                 time = erlang:localtime()}).

-record(ring_opt, { name                     :: atom(), 
                    node_copies     = 40     :: integer(),
                    expand_node     = true   :: boolean(),
                    match_operator  = '>='   :: '>='| '>',
                    concat_char     = ":"    :: char(),
                    copies_gen_type = weight :: weight | specific}).

-record(node_summary, { id          :: integer(), 
                        weight      :: integer(), 
                        percentage  :: float(),
                        copies_num  :: integer()}).

-endif.