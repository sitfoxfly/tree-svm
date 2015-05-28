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

module SkipNodeTree = 

  type internal StructuralDistanceType =
  | V of int * int
  | I of int
  | X
  | NA

  module internal AuxiliaryFunctions =

    let inline equalsByLabel n1 n2 = 
      let (_,l1) = Tree.data n1
      let (_,l2) = Tree.data n2
      l1 = l2

    let inline zipTheSame (a1: _ []) (a2: _ []) = 
      seq {
        for i = 0 to a1.Length - 1 do
          for j = 0 to a2.Length - 1 do
            let elem1 = a1.[i]
            let elem2 = a2.[j]
            if equalsByLabel elem1 elem2 then yield (elem1, elem2) 
      }

    let inline lambdan n lambda =
      let a = Array.zeroCreate n
      a.[0] <- 1.0
      a.[1] <- lambda
      for i = 2 to n - 1 do
        a.[i] <- a.[i - 1] * lambda
      a

    let inline pow2cov n = 
      let rec loop v = 
        if (1 <<< v) > n then v
        else loop (v + 1)
      loop 1 |> int

    let inline pow2lt n = 
      let rec loop v = 
        let p = (1 <<< v)
        if p > n then v - 1
        else loop (v + 1)
      loop 0

  type SkipNodeTree(tree: Tree<int * string>) =

    static let approximate_sn_kernel lambda maxDist maxSize adjecent ns1 ns2 =
      let common = AuxiliaryFunctions.zipTheSame ns1 ns2 |> Seq.toArray
      let n = common.Length

      let inline connectedWithWeight i j =
        if i < j && adjecent common.[i] common.[j] then lambda
        else -1.0

      let connectivity = Array2D.init n n connectedWithWeight
      let dk = Array2D.init maxSize n (fun _ _ -> -1.0)

      for j = 0 to n - 1 do
        dk.[0, j] <- lambda
      for k = 1 to maxSize - 1 do
        for i = n - 2 downto 0 do
          for j = i + 1 to n - 1 do
            let weight = connectivity.[i, j]
            if weight >= 0.0 then dk.[k, i] <- dk.[k - 1, j] * weight

      let mutable kernelVal = 0.0
      for i = 0 to maxSize - 1 do
        for j = 0 to n - 1 do
          let value = dk.[i, j]
          if value >= 0.0 then kernelVal <- kernelVal + value

      kernelVal

    static let sn_kernel lambda maxDist maxSize adjacent ns1 ns2 = 
      let common = AuxiliaryFunctions.zipTheSame ns1 ns2 |> Seq.toArray
      let n = common.Length

      let lambdan = AuxiliaryFunctions.lambdan (maxSize+1) lambda
      let neighbors = Array.init n (fun i -> seq { for j = 0 to n - 1 do if adjacent common.[i] common.[j] then yield j } |> ListSet.ofSeq)

      let rec calcKernel nodes ns size = 
        let cs = if size = 0 then nodes else nodes * ns
        match ListSet.minElement cs with
        | None -> lambdan.[size]
        | Some v -> 
          let nodes = ListSet.remove v nodes
          let woV = calcKernel nodes ns size
          let wV = 
            if size < maxSize then calcKernel nodes (neighbors.[v] + ns) (size + 1)
            else 0.0
          woV + wV
        
      let kernelValue = calcKernel (seq {0..n - 1} |> ListSet.ofSeq) ListSet.empty 0
      kernelValue - 1.0 // substract the impact of the empty subgraph

    let dfsArray = Tree.toArray tree
    let eulerArray = Tree.toEulerArray tree
    let levelArray = Tree.toEulerLevelArray 0 tree
    let n = Array.length eulerArray
    let repr = Array.init n (fun _ -> -1)
    let sizeOfSparseTable = AuxiliaryFunctions.pow2cov n
    let sparseTable = Array2D.init n sizeOfSparseTable (fun k i -> if i = 0 then levelArray.[k] else 0)

    let rec rmq i j = 
      if i > j then rmq j i
      else 
        let sz = AuxiliaryFunctions.pow2lt (j - i + 1)
        min sparseTable.[i, sz] sparseTable.[j - (1 <<< sz) + 1, sz]
        
    let lca n1 n2 =
      let (i1, _) = Tree.data n1
      let (i2, _) = Tree.data n2
      eulerArray.[rmq repr.[i1] repr.[i2]]

    let rec structuralDistance n1 n2 = 
      let (i,_) = Tree.data n1
      let (j,_) = Tree.data n2
      if i = j then X
      elif i > j then NA
      else 
        let ri = repr.[i]
        let rj = repr.[j]
        let (k,_) = Tree.data eulerArray.[rmq ri rj]
        let rk = repr.[k]
        if rk = ri then I (levelArray.[rj] - levelArray.[rk])
        elif rk = rj then failwith "ERROR: impossible condition" // just in case
        else V (levelArray.[ri] - levelArray.[rk], levelArray.[rj] - levelArray.[rk])

    do 
      let representer i n = 
        let (id,_) = Tree.data n
        if repr.[id] = -1 then repr.[id] <- i
      Array.iteri representer eulerArray

      for i = 1 to sizeOfSparseTable - 1 do
        for k = 0 to n - 1 do
          let k2 = k + (1 <<< (i - 1))
          if k2 < n then
            sparseTable.[k, i] <- min sparseTable.[k, i - 1] sparseTable.[k2, i - 1]
          else
            sparseTable.[k, i] <- sparseTable.[k, i - 1]

    member inline internal t.NodesArray = dfsArray
    member inline internal t.StructuralDistance(n1, n2) = structuralDistance n1 n2

    static member SN_Kernel(lambda, maxDist, maxSize, t1: SkipNodeTree, t2: SkipNodeTree) = 
      let inline adjacent_normally (v1, u1) (v2, u2) = 
        match t1.StructuralDistance(v1, v2) with
        | I r1 when r1 <= maxDist -> 
          match t2.StructuralDistance(u1, u2) with
          | I r2 -> r1 = r2 
          | _ -> false
        | V (l1, r1) when l1 + r1 <= maxDist -> 
          match t2.StructuralDistance(u1, u2) with
          | V (l2, r2) -> l1 = l2 && r1 = r2
          | _ -> false
        | _ -> false
      sn_kernel lambda maxDist maxSize adjacent_normally t1.NodesArray t2.NodesArray

    static member LSN_Kernel(lambda, maxDist, maxSize, t1: SkipNodeTree, t2: SkipNodeTree) =
      let inline adjecent_linearly (v1, u1) (v2, u2) = 
        match t1.StructuralDistance(v1, v2) with
        | I r1 when r1 <= maxDist -> 
          match t2.StructuralDistance(u1, u2) with
          | I r2 -> r1 = r2 
          | _ -> false
        | _ -> false      
      approximate_sn_kernel lambda maxDist maxSize adjecent_linearly t1.NodesArray t2.NodesArray

    static member LASN_Kernel(lambda, maxDist, maxSize, t1: SkipNodeTree, t2: SkipNodeTree) =
      let inline adjacent_normally (v1, u1) (v2, u2) = 
        match t1.StructuralDistance(v1, v2) with
        | I r1 when r1 <= maxDist -> 
          match t2.StructuralDistance(u1, u2) with
          | I r2 -> r1 = r2 
          | _ -> false
        | V (l1, r1) when l1 + r1 <= maxDist -> 
          match t2.StructuralDistance(u1, u2) with
          | V (l2, r2) -> l1 = l2 && r1 = r2
          | _ -> false
        | _ -> false 
      approximate_sn_kernel lambda maxDist maxSize adjacent_normally t1.NodesArray t2.NodesArray

  let sn_kernel lambda maxDist maxSize t1 t2 = SkipNodeTree.SN_Kernel(lambda, maxDist, maxSize, t1, t2)

  let lsn_kernel lambda maxDist maxSize t1 t2 = SkipNodeTree.LSN_Kernel(lambda, maxDist, maxSize, t1, t2)

  let lasn_kernel lambda maxDist maxSize t1 t2 = SkipNodeTree.LASN_Kernel(lambda, maxDist, maxSize, t1, t2)

  let ofString s =
    let time = ref -1
    let before l =
      time := !time + 1
      (!time, l)

    let after n cs = n
    SkipNodeTree(Tree.ofString before after s)