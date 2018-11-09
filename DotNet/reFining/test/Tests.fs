module Tests
open refining.infr
open refining.unify
open Xunit
open Xunit.Abstractions

type Test(out: ITestOutputHelper) =
    let println it = out.WriteLine <| sprintf "%A\n" it

    [<Fact>]
    let test1 () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let state, tvar = allocate_tvar state
        let arrow_t = Op(Arrow, t1, t2)
        let arrow_t2 = Op(Arrow, t1, tvar)
        let res = unify  arrow_t arrow_t2 state
        println "errs, states: "
        println res
        Assert.True(
            match res with
            | Success _ -> true
            | _ -> false)
        
    [<Fact>]
    let test2 () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let arrow_t = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t1, t2)
        let res = unify arrow_t arrow_t2 state
        println "errs, states: "
        println res
        Assert.True(
            match res with
            | Success _ -> false
            | _ -> true)

    [<Fact>]
    let ``test generic arrow`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)
        let state, tvar = allocate_tvar state
        let arrow_t =  
            Forall(
                to_free_set [tvar], 
                Op(Arrow, tvar, tvar))
            
        let arrow_t2 = Op(Arrow, t1, t1)
        let arrow_t3 = Op(Arrow, t2, t2)
        let arrow_t4 = Op(Arrow, t3, t3)
        println state
        let res = 
            unify  arrow_t arrow_t2 state >>= fun state ->
            println state
            unify  arrow_t arrow_t3 state >>= fun state ->
            unify  arrow_t arrow_t4  state >>= fun state ->
            ok state 
        match res with
        | Success states  ->
            println states
            true 
        | Fail errs ->
            println errs
            false
        |> Assert.True

    [<Fact>]
    let ``test generic arrow 2`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)

        let state, tvar = allocate_tvar state
        let arrow_t =  Op(Arrow, tvar, tvar)
        let arrow_t3 = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t2, t1)
        let arrow_t4 = Op(Arrow, t3, t1)
        println state
        let res = 
            unify  arrow_t arrow_t2 state >>= fun state ->
            println state
            unify  arrow_t arrow_t3 state>>= fun state ->
            println state
            unify  arrow_t arrow_t4 state>>= fun state ->
            println state
            ok state 
        match res with
        | Success states  ->
            println states
            false 
        | Fail errs ->
            println errs
            true
        |> Assert.True

    [<Fact>]
    let ``test refinement`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)
        let state, tvar = allocate_tvar state

        let arrow_t =  
            Forall(
                to_free_set [tvar], 
                Guard(Op(Arrow, tvar, tvar), 
                      Or(Eq(tvar, t1), 
                         Eq(tvar, t2))))
            
        let arrow_t1 = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t2, t2)
        let arrow_t3 = Op(Arrow, t3, t3)

        let res1 = 
            unify arrow_t arrow_t1 state >>= fun state ->
            ok state
        
        let res2 = 
            unify arrow_t arrow_t2 state >>= fun state ->
            ok state 

        let res3 = 
            unify arrow_t arrow_t3 state >>= fun state ->
            ok state

        match res1, res2, res3 with
        | Success _, Success _, Fail _ ->
            true 
        | it ->
            println it
            false
        |> Assert.True

    [<Fact>]
    let ``test implies`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)
        let state, tvar1 = allocate_tvar state
        let state, tvar2 = allocate_tvar state
        

        let arrow_t =  
            Forall(
                to_free_set [tvar1; tvar2], 
                Guard(Op(Arrow, tvar1, tvar2), 
                      Imply(Eq(tvar1, t1), Eq(tvar2, t2))))
            
        let arrow_t1 = Op(Arrow, t1, t1) // err
        let arrow_t2 = Op(Arrow, t1, t2) // ok
        let arrow_t3 = Op(Arrow, t2, t3) // ok

        let state, tvar3 = allocate_tvar state
        let state, tvar4 = allocate_tvar state
        let arrow_tn = Op(Arrow, tvar3, tvar4)

        let res1 = 
            unify arrow_t arrow_tn state >>= fun state ->
            unify arrow_tn arrow_t1 state >>= fun state ->
            ok state
        
        let res2 = 
            unify arrow_t arrow_t2 state >>= fun state ->
            ok state 

        let res3 = 
            unify arrow_t arrow_t3 state >>= fun state ->
            ok state

        match res1, res2, res3 with
        | Fail _, Success _, Success _ ->
            true

        | _ ->
            false
        |> Assert.True

    [<Fact>]
    let ``test not`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)
        let state, tvar1 = allocate_tvar state
        let state, tvar2 = allocate_tvar state
        

        let arrow_t =  
            Forall(
                to_free_set [tvar1; tvar2], 
                Guard(Op(Arrow, tvar1, tvar2), 
                      Not >> Not <| Eq(tvar1, t1)))
            
        let arrow_t1 = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t1, t2)
        let arrow_t3 = Op(Arrow, t2, t3)

        let state, tvar3 = allocate_tvar state
        let state, tvar4 = allocate_tvar state
        let arrow_tn = Op(Arrow, tvar3, tvar4)

        let res1 = 
            unify arrow_t arrow_tn state >>= fun state ->
            unify arrow_tn arrow_t1 state >>= fun state ->
            ok state
        
        let res2 = 
            unify arrow_t arrow_t2 state >>= fun state ->
            ok state 

        let res3 = 
            unify arrow_t arrow_t3 state >>= fun state ->
            ok state

        match res1, res2, res3 with
        | Success _, Success _, Fail _ ->
            true

        | _ ->
            false
        |> Assert.True

    [<Fact>]
    let ``test datatype`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)
        let L = datatype "List"
        let cons = Op(Arrow, t1, Op(Arrow, L t1, L t1))

        let state, a = allocate_tvar state
        let specialized = Op(Arrow, a, Op(Arrow, L a, L a))

        let res1 = 
            unify cons specialized state >>= fun state ->
            ok state
        
        println res1
        match res1 with
        | Success _ ->
            true

        | _ ->
            false
        |> Assert.True