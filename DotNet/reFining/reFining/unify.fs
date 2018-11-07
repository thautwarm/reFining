module refining.unify
open refining.infr

let rec unify (state: state) l r =
    let state, l = prune state l
    let state, r = prune state r
    let rec unify_rec (state: state) = 
        function
        | Prim l, Prim r ->
            state {
                if l <> r then return state.fail (Prim l, Prim r) "not_equal"
                else return! state
            }
        | Op(Arrow, l1, r1), Op(Arrow, l2, r2)
            ->
            let state, l1 = free state l1
            let state, l2 = free state l2
            state {
               let! state = unify_rec state (l1, l2)
               let! state = unify_rec state (r1, r2)
               return! state
            }
        | Op(Join, l1, r1), Op(Join, l2, r2)
        | Op(Stmt, l1, r1), Op(Stmt, l2, r2)
            ->
            state {
               let! state = unify_rec state (l1, l2)
               let! state = unify_rec state (r1, r2)
               return! state
            }
        | (Ref l_id as l), (Ref r_id as r)->
            state {
                if l_id = r_id then
                    return! state
                elif occur_in state (l, r) then
                    return state.fail (l, r) ""
                else
                    return! state.write_store l_id <| Ref r_id
            }
        | Ref ref_id, r ->
            state { return! state.write_store ref_id r }
        | l, (Ref _ as r) -> unify_rec state (r, l)
        | _ as tp -> state.fail tp "unmatched"

    unify_rec state (l, r)

type HM = {
    _store: Map<int, Type>
 }
 with 
    interface state with
        member hm.store = hm._store
        
        member hm.fail (a, b) msg =
            Err(sprintf "%A(%A <> %A)" msg a b, hm)

        member hm.write_store i ty =
            upcast {hm with _store = Map.add i ty hm._store}
        
        member __.Bind ((m, mf)) =
            match m with
            | Ok state -> mf state
            | _ as m -> m
        
        member __.Return i = i
        
        member __.ReturnFrom i = Ok i

            
            
        
