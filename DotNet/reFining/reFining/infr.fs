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

type err =
| Msg_err of string
| Type_err of Type * Type
| Join_err of err * err

type 'a checking = 
| Err of err 
| Ok  of state * 'a

and state = {
    store: Map<int, Type>
    id   : int
}

let new_state() = {store = Map.ofSeq []; id = 0}

let (>>=) m mf = 
    match m with
    | Ok(state, ty) -> mf state ty
    | e -> e

let update_store_types ref_id t (store: Map<int, Type>) = 
    Map.add ref_id t store

let update_store_refs ref_id new_ref_id (store: Map<int, Type>) = 
    Map.ofSeq <| seq {
        for KV(k, v) in store do 
        if k = ref_id 
        then yield new_ref_id, v
        else yield ref_id, v
    }

let allocate_id {id=id; store = store} = 
    {store=store; id=id + 1}, id

let allocate_tvar state = 
    let state, id = allocate_id state
    state, Ref id

let free_store ref_id new_ref_id (store: Map<int, Type>): Map<int, Type> = 
    Map.ofSeq <| seq {
        for KV(k, v) in store do 
        if k = ref_id 
        then yield new_ref_id, v
        yield ref_id, v
    }
   

let rec prune t (state: state): state * Type =
    
    match t with 
    | Prim _ as it -> state, it
    | Op(op, l, r) ->
      let state, l = prune l state
      let state, r = prune r state
      state, Op(op, l, r)
    | Ref ref_id as t ->
    let store = state.store
    match Map.tryFind ref_id store with
    | Some t ->
        let state, t = prune t state
        {state with store = update_store_types ref_id t store}
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
    let ref_ids = get_frees t
    let state, auto_comp = 
        List.fold
        <| fun (state, lst) ref_id ->
            let state, new_ref_id = allocate_id state
            state, (ref_id, new_ref_id) :: lst
        <| (state, [])
        <| (List.ofSeq ref_ids)
    state, Map.ofList auto_comp
    


let free state auto_comp t: Type =
    let rec free =
        function
        | Prim _ as t -> t
        | Op(op, l, r) ->
            let l = free l
            let r = free r
            Op(op, l, r)
        | Ref ref_id as t ->
        match Map.tryFind ref_id auto_comp with
        | Some new_ref_id ->            
            Ref new_ref_id
        | _ -> 
        match Map.tryFind ref_id state.store with
        | Some t -> free t
        | None   -> t
    free t  


type occur = 
 | Just_occur 
 | Recur_occur
 | No_occur

let occur_in (state: state) a b =
    let state, a = prune a state
    let state, b = prune b state
    match a with
    | Op _ | Prim _ -> state, No_occur
    | Ref a ->
    let rec contains = 
        function
        | Ref ref_id when a = ref_id ->
            Just_occur
        | Ref ref_id ->
            match Map.tryFind ref_id state.store with
            | Some t -> contains t
            | None -> No_occur
        | Op(_, l, r) -> 
            if contains l <> No_occur || contains r <> No_occur then
                Recur_occur
            else No_occur
        | _ -> No_occur
    state, contains b





        