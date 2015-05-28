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

module SubSetTree =

  type SubSetTree(tree: Tree<int * string * string>) =

    static let sst_delta sigma lambda (cache: double [,]) t1 t2 = 
      let rec inner_sst_delta t1 t2 =
        let (Node((id1,_,_),ch1)) = t1
        let (Node((id2,_,_),ch2)) = t2
        let cachedVal = cache.[id1, id2]
        if cachedVal >= 0.0 then cachedVal
        else 
          if ch1.IsEmpty && ch2.IsEmpty then
            cache.[id1, id2] <- lambda
            lambda
          else
            let calc (n1, n2) =
              let (_,_,p1) = Tree.data n1
              let (_,_,p2) = Tree.data n2
              if p1 = p2 then sigma + inner_sst_delta n1 n2
              else sigma
            let delta = Seq.zip ch1 ch2 |> Seq.map calc |> Seq.fold (*) 1.0
            cache.[id1, id2] <- delta
            delta
      inner_sst_delta t1 t2

    let orderedList = tree |> Tree.toSeq |> Seq.sortBy (fun (Node((_,_,p),_)) -> p) |> Seq.toList
    let length = orderedList.Length

    member inline internal t.OrderedList = orderedList
    member inline internal t.Length = length

    static member inline internal ST_SST_Kernel(sigma, delta, t1: SubSetTree, t2: SubSetTree) =
      let o1 = t1.OrderedList
      let o2 = t2.OrderedList
      let cache = Array2D.init t1.Length t2.Length (fun _ _ -> -1.0)
      let nodes = ListUtils.zipTheSame (fun (Node((_,_,k),_)) -> k) o1 o2
      nodes |> Seq.map (fun (n1, n2) -> sst_delta sigma delta cache n1 n2) |> Seq.sum

    static member SST_Kernel(lambda, t1: SubSetTree, t2: SubSetTree) = SubSetTree.ST_SST_Kernel(1.0, lambda, t1, t2)

    static member ST_Kernel(lambda, t1: SubSetTree, t2: SubSetTree) = SubSetTree.ST_SST_Kernel(0.0, lambda, t1, t2)

  let st_kernel lambda t1 t2 = SubSetTree.ST_Kernel(lambda, t1, t2)

  let sst_kernel lambda t1 t2 = SubSetTree.SST_Kernel(lambda, t1, t2)

  let ofString s =
    let time = ref -1
    let before l =
      time := !time + 1
      (!time, l)

    let after (t, l) cs =
      let folder acc x = 
        let (_,l,_) = Tree.data x
        match acc with
        | "" -> l
        | acc -> acc + "|" + l
      let production = l + "->" + List.fold folder "" cs
      (t, l, production)

    SubSetTree(Tree.ofString before after s)