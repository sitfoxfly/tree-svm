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

module InterTree =

  type InterTree(tree: Tree<string>) =

    static let inter_kernel (o1: _ []) (o2: _ []) =
      let n1 = o1.Length
      let n2 = o2.Length
      if n1 = 0 || n2 = 0 then 0
      else
        let rec scan i j state acc =
          if i = n1 then acc
          else
            if j = n2 then 
              match state with
              | Some jj -> scan (i+1) jj None acc
              | None -> acc
            else
              let elem1 = o1.[i]
              let elem2 = o2.[j]
              match state with
              | Some jj -> 
                if elem1 = elem2 then scan i (j+1) state (1+acc)
                else scan (i+1) jj None acc
              | None ->
                if elem1 < elem2 then scan (i+1) j state acc
                elif elem1 > elem2 then scan i (j+1) state acc
                else scan i (j+1) (Some j) (1+acc)

        scan 0 0 None 0

    let orderedLabels = tree |> Tree.toDataSeq |> Seq.toArray 

    do
      Array.sortInPlace orderedLabels

    member inline internal t.OrderedLabels = orderedLabels

    static member InterKernel(t1: InterTree, t2: InterTree) = inter_kernel (t1.OrderedLabels) (t2.OrderedLabels) |> double

  let inter_kernel t1 t2 = InterTree.InterKernel(t1, t2)

  let ofString s = InterTree(Tree.ofString id (fun x _ -> x) s)