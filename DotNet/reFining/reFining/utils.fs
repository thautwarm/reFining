module refining.utils
type arraylist<'e> = System.Collections.Generic.List<'e>
type hashdict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let (|KV|) (kvpair: System.Collections.Generic.KeyValuePair<'k, 'v>) = 
    KV (kvpair.Key, kvpair.Value)

type missing_dict<'k, 'v when 'k: equality>(factory) = 
        
    let _d = hashdict<'k, 'v>()
    
    member __.get key = 
        if _d.ContainsKey key then
           _d.[key]
        else
        let v = factory key
        _d.[key] <- v
        v
    
    
    
  