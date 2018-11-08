module refining.unify
open refining.infr

let rec unify (state: state) l r =
    let rec unify_rec l r (state: state) = 
        let state, l = prune l state
        let state, r = prune r state
        match (l, r) with
        | (Prim _ as l), (Prim _ as r)->
            if l <> r then 
                let msg = Msg_err "not_equal"
                let mismatch = Type_err(l, r)
                let join = Join_err(msg, mismatch)
                Err join
            else Ok(state, l)
            
        | Op(Forall, a1, a2), b
        | b, Op(Forall, a1, a2) ->
            let state, frees = get_frees state a1
            let a2 = free state frees a2
            unify_rec a2 b state

        | Op(op1, l1, r1), Op(op2, l2, r2) when op1 = op2 ->
            unify_rec l1 l2 state >>= fun state l ->
            unify_rec r1 r2 state >>= fun state r ->
            let opr = Op(op1, l, r)
            Ok(state, opr)
        | (Ref l_id as l), (Ref r_id as r)->
            if l_id = r_id then
                Ok(state, l)
            else
            let state, occur = occur_in state l r
            if occur = Recur_occur then
                let msg = Msg_err "recursive"
                Err msg
            else
            let state = {
                state with 
                    store = update_store_types l_id r state.store
            }
            Ok(state, l)
        | Ref ref_id, r ->
            let state = {
                state with 
                    store = update_store_types ref_id r state.store
            }
            Ok(state, r)

        | l, (Ref _ as r) -> unify_rec r l state
        | l, r -> 
        let mismatch = Type_err(l, r)
        let msg = Msg_err "unsolved"
        Err <| Join_err(msg, mismatch)
    unify_rec l r state
            
        
