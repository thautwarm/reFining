module refining.infr
open refining.utils
type Prim = 
| Int
| Float

type TypeOp = 
| Arrow
| Join
| Stmt

// there should be 2 kinds of Ref in order to avoid value restriction.
type Type = 
| Prim of Prim
| Op   of TypeOp * Type * Type
| Ref  of int


type store_env = Map<int, Type>

type checking = 
| Err of string * state
| Ok of state

and state = interface
    abstract member store: store_env
    abstract member fail: Type * Type -> string -> checking
    abstract member write_store: int -> Type -> state
    abstract member reset_store: store_env -> state
    abstract member Bind: (checking * (state -> checking)) -> checking
    abstract member Return: checking -> checking
    abstract member ReturnFrom: state -> checking
    abstract member allocate_id: int
    abstract member allocate_tvar: Type
end

let rec prune (state: state) =
    function 
    | Prim _ as it -> state, it
    | Op(op, l, r) ->
      let state, l = prune state l
      let state, r = prune state r
      state, Op(op, l, r)
    | Ref ref_id as t ->
    let store = state.store
    match Map.tryFind ref_id store with
    | Some t ->
        let state, t = prune state t
        state.write_store ref_id t
        , t
    | None -> state, t

let get_frees (state: state) t =
    let rec get_frees = 
        function
        | Prim _ as it -> Set.empty
        | Op(_, l, r) -> 
            Set.union <| get_frees l <| get_frees r
        | Ref ref_id ->
            match Map.tryFind ref_id state.store with
            | Some t -> get_frees t
            | _ -> set [ref_id]
    get_frees t


let free (state: state) (ref_ids: Set<int>) t =
    let auto_comp = missing_dict (fun _ -> state.allocate_id)
    let rec free (state: state) =
        function
        | Prim _ as t -> state, t
        | Op(op, l, r) ->
            let state, l = free state l
            let state, r = free state r
            state, Op(op, l, r)
        | Ref ref_id->
        if ref_ids.Contains ref_id then
            let new_ref_id = auto_comp.get ref_id
            let store = state.store
            let new_store =
                Map.ofSeq <| seq {
                    for KV(k, v) in store do 
                    if k = ref_id 
                    then yield new_ref_id, v
                    yield ref_id, v
                }
            let state = state.reset_store new_store
            state, Ref new_ref_id
        else
        free state <| state.store.[ref_id]
    free state t  


let occur_in (state: state) a b =
    let undecided = get_frees state a
    let rec contains = 
        function
        | Ref ref_id when undecided.Contains ref_id ->
            true
        | Ref ref_id ->
            match Map.tryFind ref_id state.store with
            | Some t -> contains t
            | None -> false
        | Op(_, l, r) -> contains l || contains r
        | _ -> false
    contains b





        