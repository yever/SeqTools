namespace SeqTools

open System
open System.Linq
open FSharpx.Collections


module Seq =

  let inline count i = 
    seq { let n = ref i
          while true do 
            yield !n
            n := !n + 1 }

  let inline cycle (source: 'T seq) = 
    match List.ofSeq source with
      | []    -> source
      | saved -> seq { while true do yield! saved }

  let inline repeat elem n : 'T seq = 
    Enumerable.Repeat (elem, n)

  let accumulate f (source : seq<'T>) = 
    use ie = source.GetEnumerator ()
    if ie.MoveNext () 
    then source
    else seq { let total = ref ie.Current
               yield !total
               while ie.MoveNext () do
                  total := f !total ie.Current
                  yield !total }

  let inline chain collection = 
    Seq.concat collection

  let inline compress data selectors =
    selectors |> Seq.map2 (fun d s -> if s then Some d else None) data 
              |> Seq.choose id
  
  let inline product f source1 source2 = 
    seq { for x in source1 do for y in Seq.cache source2 do yield f x y }

  let inline appendItem col item = Seq.append col [item]
  
  let nProduct (source: 'T seq) r =
    repeat (List.ofSeq source) r
    |> Seq.fold (product (fun result pool -> Queue.conj pool result))
                (seq [Queue.empty])
  
  let permutations (source: 'T seq) r =
    let pool = Array.ofSeq source
    let n = pool.Length
    let r = defaultArg r n
    nProduct {0 .. n-1} r
    |> Seq.collect
        (fun indices -> seq { if Seq.length (Seq.distinct indices) = r
                              then yield [| for i in indices -> pool.[i] |] })

  let private isSorted source = 
    source |> Seq.pairwise 
           |> Seq.forall (fun (a, b) -> a <= b)
  
  let combinations (source: 'T seq) r =
    let pool = Array.ofSeq source
    permutations {0 .. pool.Length-1} (Some r)
    |> Seq.filter isSorted
    |> Seq.map (fun indices -> [| for i in indices -> pool.[i] |])

  let combinationsWithReplacement (source: 'T seq) r =
    let pool = Array.ofSeq source
    nProduct {0 .. pool.Length-1} r
    |> Seq.filter isSorted
    |> Seq.map (fun indices -> [| for i in indices -> pool.[i] |])
