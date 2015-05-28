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

module SMO = 

  type TrainingSource = { 
    IJKernel: int -> int -> float;
    y: int [];
  }
  
  type TaskParams = {
    C : double;
    NumIter : int;
  }
  
  let SMO_WSS3 task data =
    printf "Training..."
    let len = data.y.Length

    let eps = 1e-3
    let tau = 1e-12

    let C = task.C
    let numIter = task.NumIter

    let kernel = data.IJKernel
    let y = data.y

    let inline yy i = double y.[i]

    let inline Q i j = (yy i) * (yy j) * (kernel i j)

    let G = Array.init len (fun _ -> -1.0)
    let A = Array.zeroCreate len

    let selectWorkingSet () = 
      let mutable i = -1
      let mutable Gmax = -infinity
      let mutable Gmin = infinity

      for t = 0 to len-1 do
        let yt = yy t
        let At = A.[t]
        if (yt = +1.0 && At < C) || (yt = -1.0 && At > 0.0) then
          let nG = -yt * G.[t]
          if nG >= Gmax then
            i <- t
            Gmax <- nG

      let mutable j = -1
      let mutable obj_min = infinity
      for t = 0 to len - 1 do
        let yt = yy t
        let At = A.[t]
        if (yt = +1.0 && At > 0.0) || (yt = -1.0 && At < C) then
          let nG = -yt * G.[t]
          let b = Gmax - nG
          if nG <= Gmin then
            Gmin <- nG
          if b > 0.0 then
            let a =
              let value = (Q i i) + (Q t t) - 2.0 * (yy i) * yt * (Q i t)
              if value <= 0.0 then tau
              else value
            let nObj = - b * b / a
            if nObj <= obj_min then
              j <- t
              obj_min <- nObj

      if Gmax - Gmin < eps then (-1, -1)
      else (i, j)

    let rec optimizationLoop it =
      if it = 0 then ()
      else
        let (i, j) = selectWorkingSet ()
        if j = -1 then ()
        else
          let a = 
            let value = (Q i i) + (Q j j) - 2.0 * (yy i) * (yy j) * (Q i j)
            if value <= 0.0 then tau
            else value
          let b = - (yy i) * G.[i] + (yy j) * G.[j]

          let oldAi = A.[i]
          let oldAj = A.[j]
          A.[i] <- oldAi + (yy i) * b / a
          A.[j] <- oldAj - (yy j) * b / a

          let sum = (yy i) * oldAi + (yy j) * oldAj
          if A.[i] > C then A.[i] <- C
          if A.[i] < 0.0 then A.[i] <- 0.0
          A.[j] <- (yy j) * (sum - (yy i) * A.[i])
          if A.[j] > C then A.[j] <- C
          if A.[j] < 0.0 then A.[j] <- 0.0
          A.[i] <- (yy i) * (sum - (yy j) * A.[j])

          let deltaAi = A.[i] - oldAi
          let deltaAj = A.[j] - oldAj
          for t = 0 to len - 1 do
            G.[t] <- G.[t] + (Q t i) * deltaAi + (Q t j) * deltaAj

          eprintf "."
          optimizationLoop (it-1)

    optimizationLoop numIter
    eprintfn ""
    A