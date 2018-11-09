module refining.unify
open refining.utils
open refining.infr


let ok state = 
    [], [state]

let err err = 
    [err], []

let (|Fail|Success|) (errs, states) =
    match states with
    | [] -> Fail errs
    | _ -> Success states 
      
let rec fit_cond cond state = 
    match cond with
    | Eq(a, b) -> 
        unify a b state >>= fun state ->
        ok state
    | And(a, b) ->
        fit_cond a state >>= fun state ->
        fit_cond b state >>= fun state -> 
        ok state
    | Or(a, b) ->
        let errs1, states1 = fit_cond a state
        let errs2, states2 = fit_cond b state
        errs1 @ errs2, states1 @ states2
    | Not a ->
        ok {state with negations = a :: state.negations}
    | Imply(a, b) ->
        let errs, states = 
            fit_cond a state >>= fun state' ->
            fit_cond b state' >>= fun state' -> 
            ok state'
        let state = {
            state with
                negations = a :: state.negations
        }
        errs, state :: states

and unify l r state = 
    let rec unify l r (state: state) = 
        let state, l = prune l state
        let state, r = prune r state
        match (l, r) with
        
        | (Prim _ as l), (Prim _ as r)->
            if l <> r then
                let msg = Msg_err "not_equal"
                let mismatch = Type_err(l, r)
                let join = Join_err(msg, mismatch)
                err join
            else ok state
        | DataType(s1, t1), DataType(s2, t2) when s1 = s2 ->
            unify t1 t2 state
        | Guard(t, cond), other
        | other, Guard(t, cond) ->
            fit_cond cond state >>= fun state ->
            unify t other state >>= fun state ->
            ok state
            
        | Forall(frees, a2), b
        | b, Forall(frees, a2) ->
            let state, auto_comp = frees_to_comp_map frees state
            let a2 = free state auto_comp a2
            unify a2 b state

        | Op(op1, l1, r1), Op(op2, l2, r2) when op1 = op2 ->
            unify l1 l2 state >>= fun state ->
            unify r1 r2 state >>= fun state ->
            let opr = Op(op1, l, r)
            ok state

        | (Ref l_id as l), (Ref r_id as r)->
            if l_id = r_id then
                ok state
            else
            let state, occur = occur_in state l r
            if occur = Recur_occur then
                let msg = Msg_err "recursive"
                err msg
            else
            let state = {
                state with 
                    store = update_store_types l_id r state.store
            }
            ok state
        | Ref ref_id, r ->
            let state = {
                state with 
                    store = update_store_types ref_id r state.store
            }
            ok state

        | l, (Ref _ as r) -> unify r l state
        | l, r -> 
        let mismatch = Type_err(l, r)
        let msg = Msg_err "unsolved"
        err <| Join_err(msg, mismatch)
    
    let rec resolve_negation (state: state) =
        let negations = state.negations
        if List.isEmpty negations then 
            ok state
        else
        let state = {state with negations = []}
        let its = 
            List.filter 
            <| fun neg ->
                match fit_cond neg state >>= resolve_negation with
                | Success _ -> true
                | _         -> false
            <| negations
        if List.isEmpty its then
            ok state
        else
        let e = 
            List.reduce 
             <| fun a b -> Join_err(a, b)
             <| List.map Cond_err its
        err e
        
    unify l r state >>= resolve_negation