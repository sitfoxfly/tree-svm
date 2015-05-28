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

module ListUtils = 

  let zipTheSame (key: 'a -> 'b) (o1: 'a list) (o2: 'a list) =
    let rec localZip x1 k1 o acc =
      match o with
      | [] -> acc
      | x2::xs ->
        let k2 = key x2 
        if k1 <> k2 then acc
        else localZip x1 k1 xs ((x1, x2) :: acc)

    let rec zip o1 o2 acc =
      match (o1, o2) with
      | ([], _) | (_, []) -> acc
      | ((x1::xs1), (x2::xs2)) ->
        let k1 = key x1 
        let k2 = key x2
        if k1 < k2 then zip xs1 o2 acc
        elif k1 > k2 then zip o1 xs2 acc
        else
          let acc = (x1, x2) :: acc
          let acc = localZip x1 k1 xs2 acc
          zip xs1 o2 acc

    zip o1 o2 []

