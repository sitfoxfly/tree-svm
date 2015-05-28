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

module Kernel = 

  let inline normCachedIJKernel kernel (cache: double [,]) i j =
    if i = j then 1.0 // as we do not use diagonal let's save there sqrt(Kii) 
    else
      let i, j = if i < j then j, i else i, j
      let cachedValue = cache.[i, j]
      if cachedValue >= 0.0 then cachedValue
      else
        let inline getCachedSqrtKii i (cache: double [,]) =
          let cKii = cache.[i, i]
          if cKii >= 0.0 then cKii
          else
            let v = sqrt (kernel i i)
            cache.[i, i] <- v
            v
        let sqrtKii = getCachedSqrtKii i cache
        let sqrtKjj = getCachedSqrtKii j cache
        let kij = kernel i j
        let value = kij / (sqrtKii * sqrtKjj)
        cache.[i, j] <- value
        value

  let inline normKernel kernel x y =
    let kxx = kernel x x
    let kyy = kernel y y
    let kxy = kernel x y
    kxy / sqrt (kxx * kyy)

  