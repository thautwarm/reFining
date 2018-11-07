module refining.utils
type arraylist<'e> = System.Collections.Generic.List<'e>
type hashdict<'k, 'v> = System.Collections.Generic.Dictionary<'k, 'v>

let (|KV|) (kvpair: System.Collections.Generic.KeyValuePair<'k, 'v>) = 
    KV (kvpair.Key, kvpair.Value)
