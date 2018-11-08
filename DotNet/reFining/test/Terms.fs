module Terms
open refining.infr
type num =
| Int_num of int
| Float_num of float

type Term = 
| App of Term * Term
| Lam of Term * Term
| Sym of string
| Num of num
| Comb of Term * Term