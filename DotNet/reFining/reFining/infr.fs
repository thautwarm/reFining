module refining.infr
open refining.utils
open System

type prim = 
| Int
| Float

and cond =
| Or of cond * cond
| And of cond * cond
| Imply of cond * cond
| Not of cond
| Eq  of T * T

and type_op = 
| Arrow
| Join
| Stmt

and signature = {
    name: string
}

and T = 
| Prim     of prim
| Op       of type_op * T * T
| Ref      of int
| Forall   of int Set * T 
| Guard    of T * cond
| DataType of signature * T

type err =
| Msg_err of string
| Type_err of T * T
| Cond_err of cond
| Join_err of err * err
| Err

and checking = err list * state list // many states

and state = {
    store     : Map<int, T>
    negations : cond list 
    id        : int
}

let new_state() = {store=Map.ofSeq []; id=0; negations=[]}

let check_all mf : 'a list -> checking =
    let rec impl = 
        function
        | [] -> [], []
        | x :: xs ->
        let errs1, x = mf x
        let errs2, xs = impl xs
        errs1 @ errs2, x @ xs
    impl 



let (>>=) (m: checking) (mf: state -> checking) = 
    let errs, states = m
    let errs', states = check_all mf states
    errs @ errs', states
    
let update_store_types ref_id t (store: Map<int, T>) = 
    Map.add ref_id t store

let update_store_refs ref_id new_ref_id (store: Map<int, T>) = 
    Map.ofSeq <| seq {
        for KV(k, v) in store do 
        if k = ref_id 
        then yield new_ref_id, v
        else yield ref_id, v
    }

let allocate_id ({id = id} as state) =
    {state with id=id + 1}, id

let allocate_tvar state = 
    let state, id = allocate_id state
    state, Ref id

let free_store ref_id new_ref_id (store: Map<int, T>): Map<int, T> = 
    Map.ofSeq <| seq {
        for KV(k, v) in store do 
        if k = ref_id 
        then yield new_ref_id, v
        yield ref_id, v
    }
   

let rec prune t (state: state): state * T =
    match t with 
    | Prim _ -> state, t
    | DataType(sig', t) ->
        let state, t = prune t state
        state, DataType(sig', t)
    | Forall(int_set, t) ->
        // assert
        if 
            Set.exists 
            <| state.store.ContainsKey
            <| int_set
        then failwithf 
              "each of forall type var list cannot be contained in store."
        let state, t = prune t state
        state, Forall(int_set, t)

    | Guard(t, cond) ->
        let (|Cond|Else|) cond = 
            match cond with
            | Imply(a, b) -> Cond(Imply, a, b)
            | And(a, b) -> Cond(And, a, b)
            | Or(a, b) -> Cond(Or, a, b)
            | Eq _ | Not _ -> Else 

        let rec map cond state = 
            match cond with 
            | Eq(t1, t2) -> 
                let state, t1 = prune t1 state
                let state, t2 = prune t2 state
                state, Eq(t1, t2)
            | Not a -> 
                let state, a = map a state
                state, Not a
            | Cond(cons, a, b) ->
                let state, a = map a state
                let state, b = map b state
                state, cons(a, b)
            | Else -> failwith "impossible"
        let state, t = prune t state
        let state, cond = map cond state
        state, Guard(t, cond)
    | Op(op, l, r) ->
      let state, l = prune l state
      let state, r = prune r state
      state, Op(op, l, r)
    | Ref ref_id as t ->
    let store = state.store
    match Map.tryFind ref_id store with
    | Some t ->
        let state, t = prune t state
        {state with store = update_store_types ref_id t store},
        t
    | None -> state, t

let frees_to_comp_map ref_ids state =
    let state, auto_comp = 
        List.fold
        <| fun (state, lst) ref_id ->
            let state, new_ref_id = allocate_id state
            state, (ref_id, new_ref_id) :: lst
        <| (state, [])
        <| (List.ofSeq ref_ids)
    state, Map.ofList auto_comp


let free state auto_comp t: T =
    let rec free =
        function
        | Prim _ as t -> t
        | DataType(sig', t) ->
            let t = free t
            DataType(sig', t)
        | Forall(int_set, t) ->
            Forall(int_set, free t)
        | Op(op, l, r) ->
            let l = free l
            let r = free r
            Op(op, l, r)
        | Guard(t, cond) ->
            let rec map = 
                function
                | Eq(t1, t2) -> Eq(free t1, free t2)
                | Not a -> Not <| map a
                | Imply(a, b) ->
                    Imply(map a, map b)
                | Or(a, b) ->
                    Or(map a, map b)
                | And(a, b) ->
                    And(map a, map b)
            Guard(free t, map cond)
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
    | Guard _ | Forall _ | Op _ | Prim _ | DataType _ -> state, No_occur
    | Ref a ->
    let rec contains = 
        function
        | Ref ref_id when a = ref_id ->
            Just_occur
        | Ref ref_id ->
            match Map.tryFind ref_id state.store with
            | Some t -> contains t
            | None -> No_occur
        | Guard(t, _) | Forall(_, t) ->
            // TODO
            contains t
        | Op(_, l, r) -> 
            if contains l <> No_occur || contains r <> No_occur then
                Recur_occur
            else No_occur
        | _ -> No_occur
    state, contains b

let to_free_set tvars = 
    Set.ofList [
        for tvar in tvars -> 
        match tvar with 
        | Ref i -> i 
        | a -> failwithf "invalid TVar %A" a
    ]

let datatype name ty_arg = 
    DataType({name = name}, ty_arg)
  