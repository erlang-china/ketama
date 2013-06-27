# ketama
---

## About ketama 
(quote from github : https://github.com/RJ/ketama)

We wrote ketama to replace how our memcached clients mapped keys to servers.
Previously, clients mapped keys->servers like this:

    server = serverlist[hash(key)%serverlist.length];

This meant that whenever we added or removed servers from the pool, everything
hashed to different servers, which effectively wiped the entire cache.

Ketama solves this problem in the following way:

 * Take your list of servers (eg: 1.2.3.4:11211, 5.6.7.8:11211, 9.8.7.6:11211)
 * Hash each server string to several (100-200) unsigned ints
 * Conceptually, these numbers are placed on a circle called the continuum.
   (imagine a clock face that goes from 0 to 2^32)
 * Each number links to the server it was hashed from, so servers appear
   at several points on the continuum, by each of the numbers they hashed to.
 * To map a key->server, hash your key to a single unsigned int, and find the
   next biggest number on the continuum. The server linked to that number is
   the correct server for that key.
 * If you hash your key to a value near 2^32 and there are no points on the
   continuum greater than your hash, return the first server in the continuum.


## About this ketama libary

We could dynamic set up the node weight and node copies, and you could save your current options in stash, 
and you can recovery your options at any time, you can also make an balance test to make sure the node weight are close to the actual situation


### Ring 

```erlang
-record(ring_opt, { name                     :: atom(), 
                    node_copies     = 40     :: integer(),
                    expand_node     = true   :: boolean(),
                    match_operator  = '>='   :: '>='| '>',
                    concat_char     = ":"    :: char(),
                    copies_gen_type = weight :: weight | specific}).

```

 **name:** 
 your ring name

**node_copies:**  
only work when the copies_gen_type is weight, mutilple of nodes.

**expand_node:** 
the hash value of md5(key) was grouped by 16 bytes, and each four byte is a node hash key,
if we only want use first 4 bytes, just disable this option.

**match_operator:**  
an operator about ets match_spec.

**concat_char:**  
if your hashseed is "ip:port", the copies hashseed is `ip:port`+ `concat_char` + `copy_id`

**copies_gen_type:**
weight:auto compute the copies number
specific: manually set the copies number, but if the `expand_node = true`, the copies number will be quadrupled

#### Usage:

```erlang
RingTest = #ring_opt{name = test, node_copies=80}.
    #ring_opt{name = test,node_copies = 80,expand_node = true,
              match_operator = '>=',concat_char = ":",
              copies_gen_type = weight}

(ketama@ZhengXujin-PC)6> ketama:add_ring(RingTest).
    ok

(ketama@ZhengXujin-PC)12> ketama:list_rings().
[#ring_opt{name = test,node_copies = 80,expand_node = true,
           match_operator = '>=',concat_char = ":",
           copies_gen_type = weight}]
           
(ketama@ZhengXujin-PC)13> ketama:get_ring_summary(test).
    [#node_summary{id = 1,weight = 10,percentage = 0.2,
                   copies_num = 64},
     #node_summary{id = 2,weight = 40,percentage = 0.8,
                   copies_num = 256}]
```

 
### Node 

```erlang
-record(node, {id             :: integer(), 
               hash_seed      :: string(),
               weight         :: integer(),
               copies_num     :: integer(),
               object         :: any()}).
```
**id:**
nodeid

**hash_seed:**
e.g.: "127.0.0.1:8080".

**weight:**
only work while `copies_gen_type = weight`.

**copies_num:**
only work while `copies_gen_type = weight`.

**object:**
your customize value, the ketama:get_object(ringname, key) will return {ok, {NodeId, Object}}, then you can store your own data.


#### Usage

```erlang
Node1 = #node{id=1, hash_seed = "127.0.0.1:8080", weight=10, object={a,b,c}}.
Node2 = #node{id=2, hash_seed = "127.0.0.1:8081", weight=40, object={a,b,e}}.
ketama:add_node(test, Node1).
ketama:add_node(test, Node2).
ketama:set_node_weight(test,1,100). %% work will gen_type == weight
ketama:set_node_copies(test,1,100). %% work will gen_type == specific
```

### Stash

#### What is stash?

while you do some A/B testing about request, you may reduce the weight and add an node, then the tiny request will goto the testing server, 
but you also want to backup your current options that after the testing was finished, you could recovery by a stash.

```erlang
-record(stash, { id    :: atom(),
                 ring  :: atom(), 
                 name  :: atom(), 
                 nodes :: list(),
                 time = erlang:localtime()}).
```

#### Usage:

```erlang
ketama:stash(test,t1). 
ketama:stash_apply(test,t1). 
ketama:remove_stash(test,t1). 
ketama:remove_stashes(test). 
ketama:list_stashes(). 
ketama:list_stashes(test). 
ketama:get_stash(test,t1).
```


### Balance Test

#### What is balance test?
while you set up the weight of the node, you may ask a question, do really this weight works fine?
So, lets do a balance test!

#### Usage:
```erlang
ketama:balance_test(ring_name, TestKeyNumber).
```

you'd better set a big `TestKeyNumber`, for example : 1000000, then this function will give are real node hit report like this:

```erlang
(ketama@ZhengXujin-PC)11> ketama:get_ring_summary(test).
[#node_summary{id = 1,weight = 10,percentage = 0.2,
               copies_num = 64},
 #node_summary{id = 2,weight = 40,percentage = 0.8,
               copies_num = 256}]


(ketama@ZhengXujin-PC)10> ketama:balance_test(test, 1000).
{ok,[{2,{781,0.781}},{1,{219,0.219}}]}
```
it means node 1 hitted 219 times, and the percentage is 21.9%, the node 2 hitted 781 times, and the percentage is 78.1%.
So, lets look at the configurate(ketama:get_ring_summary), the results are very similar.

**note:** if you find the resaults are not smimilar to your configure, you should increase your node_copies(weight mode) or copies_num(specific mode)