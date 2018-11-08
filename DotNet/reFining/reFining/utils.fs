module refining.utils

let (|KV|) (kvpair: System.Collections.Generic.KeyValuePair<'k, 'v>) = 
    KV (kvpair.Key, kvpair.Value)
    
    
    
  