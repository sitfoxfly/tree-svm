// Copyright 2015 Singapore Management University (SMU). All Rights Reserved. 
//
// Permission to use, copy, modify and distribute this software and 
// its documentation for purposes of research, teaching and general
// academic pursuits, without fee and without a signed licensing
// agreement, is hereby granted, provided that the above copyright
// statement, this paragraph and the following paragraph on disclaimer
// appear in all copies, modifications, and distributions.  Contact
// Singapore Management University, Intellectual Property Management
// Office at iie@smu.edu.sg, for commercial licensing opportunities.
//
// This software is provided by the copyright holder and creator "as is"
// and any express or implied warranties, including, but not Limited to,
// the implied warranties of merchantability and fitness for a particular 
// purpose are disclaimed.  In no event shall SMU or the creator be 
// liable for any direct, indirect, incidental, special, exemplary or 
// consequential damages, however caused arising in any way out of the
// use of this software.

namespace TreeSVM

module PartialTree = 

  type PartialTree(tree: Tree<int * string>) =

    static let pt_delta lambda lambda2 mu (cache: double [,]) t1 t2 =
      let rec inner_pt_delta t1 t2 =
        let rec sk_delta c1 c2 =
          let ac1 = Array.ofList c1
          let ac2 = Array.ofList c2
          let n = ac1.Length
          let m = ac2.Length
          let p = min n m
          let dps = Array2D.init (n+1) (m+1) (fun _ _ -> 0.0)
          let dp = Array2D.init (n+1) (m+1)  (fun _ _ -> 0.0)
          let k = Array.zeroCreate (n+1)

          for i = 1 to n do
            for j = 1 to m do
              let ni = ac1.[i-1]
              let nj = ac2.[j-1]
              let (_,l1) = Tree.data ni
              let (_,l2) = Tree.data nj
              if l1 = l2 then
                let v = inner_pt_delta ni nj
                dps.[i, j] <- v
                k.[0] <- k.[0] + v
              else
                dps.[i, j] <- 0.0

          for l = 1 to p do
            for j = 0 to m do dp.[l-1, j] <- 0.0
            for i = 0 to n do dp.[i, l-1] <- 0.0
            for i = l to n do
              for j = l to m do
                let ni = ac1.[i-1]
                let nj = ac2.[j-1]
                let (_,l1) = Tree.data ni
                let (_,l2) = Tree.data nj

                dp.[i, j] <- dps.[i, j] + lambda * dp.[i-1, j] + lambda * dp.[i, j-1] - lambda2 * dp.[i-1, j-1]

                if l1 = l2 then
                  let v = dp.[i-1, j-1] * inner_pt_delta ni nj
                  dps.[i, j] <- v
                  k.[l] <- k.[l] + v

          Array.sum k

        let (Node ((id1,l1),ch1)) = t1
        let (Node ((id2,l2),ch2)) = t2
        let cacheVal = cache.[id1, id2]
        if cacheVal >= 0.0 then cacheVal
        else
          if l1 <> l2 then
            cache.[id1, id2] <- 0.0
            0.0
          elif ch1 = [] && ch2 = [] then
            let v = mu * lambda2
            cache.[id1, id2] <- v
            v
          else
            let v = mu * (lambda2 + sk_delta ch1 ch2)
            cache.[id1, id2] <- v
            v

      inner_pt_delta t1 t2

    let orderedList = tree |> Tree.toSeq |> Seq.sortBy (fun (Node ((_,l),_)) -> l) |> Seq.toList
    let length = orderedList.Length

    member inline internal t.OrderedList = orderedList
    member inline internal t.Length = length

    static member PT_Kernel(lambda, mu, t1: PartialTree, t2: PartialTree) =
      let o1 = t1.OrderedList
      let o2 = t2.OrderedList
      let cache = Array2D.init t1.Length t2.Length (fun _ _ -> -1.0)
      let nodes = ListUtils.zipTheSame (fun (Node((_,k),_)) -> k) o1 o2
      nodes |> Seq.map (fun (n1, n2) -> pt_delta lambda (lambda * lambda) mu cache n1 n2) |> Seq.sum

  let pt_kernel lambda mu t1 t2 = PartialTree.PT_Kernel(lambda, mu, t1, t2)

  let ofString s =
    let time = ref -1
    let before l =
      time := !time + 1
      (!time, l)

    let after n cs = n
    PartialTree(Tree.ofString before after s)