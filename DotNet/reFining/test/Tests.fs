module Tests
open refining.infr
open refining.unify
open Xunit
open Xunit.Abstractions

type Test(out: ITestOutputHelper) =
    let println it = out.WriteLine <| sprintf "%A\n" it

    [<Fact>]
    let test1 () =
        let state: state = upcast {_store = Map.ofSeq []; id = 0}
        let t1 = Prim Int
        let t2 = Prim Float
        let tvar = state.allocate_tvar
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
        let state: state = upcast {_store = Map.ofSeq []; id = 0}
        let t1 = Prim Int
        let t2 = Prim Float
        let tvar = state.allocate_tvar
        let arrow_t = Op(Arrow, t1, t1)
        let arrow_t2 = Op(Arrow, t1, t2)
        let res = unify state arrow_t arrow_t2
        println res
        Assert.True(
            match res with
            | Ok _ -> false
            | _ -> true)