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

type Tree<'a> = Node of 'a * Tree<'a> list

module Tree =

  let inline data (Node(d, _)) = d

  let rec length (Node(_,cx)) =
    1 + List.fold (fun acc x -> acc + length x) 0 cx

  let rec toSeq (Node(_,cx) as t) = 
    seq {
      yield t
      for c in cx do yield! toSeq c
    } 

  let rec toDataSeq (Node(d,cx)) =
    seq {
      yield d
      for c in cx do yield! toDataSeq c
    } 

  let rec toEulerSeq (Node(_,cx) as n) = 
    seq { 
      yield n
      for c in cx do yield! toSeq c
      yield n
    }

  let rec toEulerLevelSeq l (Node(_, cx)) = 
    seq { 
      yield l
      for c in cx do yield! toEulerLevelSeq (l+1) c
      yield l
    }

  let toArray t = t |> toSeq |> Seq.toArray

  let toList t = t |> toSeq |> Seq.toList

  let toEulerArray t = t |> toEulerSeq |> Seq.toArray

  let toEulerLevelArray l t = t |> toEulerLevelSeq l |> Seq.toArray

  let (|Surrounded|_|) prefix postfix (source : string) = 
    let lastCharIndex = source.Length - 1
    if source.Length > 0 && source.[0] = prefix && source.[lastCharIndex] = postfix then 
      Some(source.Substring(1, lastCharIndex - 1))
    else None

  let rec ofString (before: string -> 'a) (after: 'a -> Tree<'b> list -> 'b) (source : string) : Tree<'b> = 
    let source = source.Trim()
    match source with
    | Surrounded '(' ')' mid -> 
      match mid.IndexOf '(' with
      | -1 ->  
        let n = mid.Trim()
        let n1 = before n
        let data = after n1 []
        Node(data, [])
      | startIndex -> 
        let rec parseChildren s xs = 
          match s with
          | "" -> List.rev xs
          | s -> 
            let rec locateChild i l = 
              if i >= s.Length then failwith ("ERROR: wrong source format: " + source)
              else 
                let change = 
                  match s.[i] with
                  | '(' -> 1
                  | ')' -> -1
                  | _ -> 0
                let newL = l + change
                if newL = 0 then i
                elif newL < 0 then failwith ("ERROR: wrong source format: " + source)
                else locateChild (i + 1) (l + change)
            
            let childLength = 1 + locateChild 0 0
            let head = s.Substring(0, childLength)
            let tail = s.Substring(childLength).Trim()
            parseChildren tail (ofString before after head :: xs)
        
        let n = mid.Substring(0, startIndex).Trim()
        let n1 = before n
        let children = parseChildren (mid.Substring(startIndex).Trim()) []
        let data = after n1 children
        Node(data, children)
    | _ -> failwith ("ERROR: wrong source format: " + source)


