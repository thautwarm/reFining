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

    abstract member Bind: (checking * (state -> checking)) -> checking
    abstract member Return: checking -> checking
    abstract member ReturnFrom: state -> checking
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

let rec free (state: state) =
    function
    | Prim _ as it -> state, it
    | _ -> raise <| System.NotImplementedException()

let rec occur_in (state: state) =
    function 
    | Prim _, Prim _ -> false
    | _ -> raise <| System.NotImplementedException()



        