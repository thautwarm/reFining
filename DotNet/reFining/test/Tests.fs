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
        let res = unify state arrow_t arrow_t2
        println res
        Assert.True(
            match res with
            | Ok _ -> true
            | _ -> false)
        
    [<Fact>]
    let test2 () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let arrow_t = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t1, t2)
        let res = unify state arrow_t arrow_t2
        println res
        Assert.True(
            match res with
            | Ok _ -> false
            | _ -> true)

    [<Fact>]
    let ``test generic arrow`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)

        let state, tvar = allocate_tvar state
        let arrow_t =  Op(Forall, tvar, Op(Arrow, tvar, tvar))
        let arrow_t2 = Op(Arrow, t1, t1)
        let arrow_t3 = Op(Arrow, t2, t2)
        let arrow_t4 = Op(Arrow, t3, t3)
        println state
        let res = 
            unify state arrow_t arrow_t2 >>= fun state t2 ->
            println t2
            unify state arrow_t arrow_t3 >>= fun state t3 ->
            println t3
            unify state arrow_t arrow_t4 >>= fun state t4 ->
            println t4
            Ok(state, t4)
        match res with
        | Ok(state, _) ->
            println state
            true 
        | Err msg ->
            println msg
            false
        |> Assert.True

    [<Fact>]
    let ``test generic arrow 2`` () =
        let state: state = new_state()
        let t1 = Prim Int
        let t2 = Prim Float
        let t3 = Op(Arrow, t1, t2)

        let state, tvar = allocate_tvar state
        let arrow_t = Op(Arrow, tvar, tvar)
        let arrow_t2 = Op(Arrow, t1, t1)
        let arrow_t3 = Op(Arrow, t2, t2)
        let arrow_t4 = Op(Arrow, t3, t1)
        println state
        let res = 
            unify state arrow_t arrow_t2 >>= fun state t2 ->
            println t2
            unify state arrow_t arrow_t3 >>= fun state t3 ->
            println t3
            unify state arrow_t arrow_t4 >>= fun state t4 ->
            println t4
            Ok(state, t4)
        match res with
        | Ok(state, _) ->
            println state
            false 
        | Err msg ->
            println msg
            true
        |> Assert.True
