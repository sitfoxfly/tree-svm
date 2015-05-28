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

[<Sealed>]
type ListSet<[<EqualityConditionalOn>] 'T when 'T : comparison> private (l : 'T list) = 
  
  let minimumElement l = 
    match l with
    | [] -> None
    | x :: xs -> Some x
  
  let rec remove y l acc = 
    match l with
    | [] -> ListSet(List.rev acc)
    | x :: xs -> 
      if x = y then ListSet(List.append (List.rev acc) xs)
      else remove y xs (x :: acc)
  
  static let empty = ListSet(List.empty)
  
  static let intersect l1 l2 = 
    let rec innerIntersect l1 l2 acc = 
      match (l1, l2) with
      | ([], _) | (_, []) -> List.rev acc
      | (x1 :: xs1, x2 :: xs2) -> 
        match (x1, x2) with
        | (v1, v2) when v1 > v2 -> innerIntersect l1 xs2 acc
        | (v1, v2) when v1 < v2 -> innerIntersect xs1 l2 acc
        | (v1, v2) -> innerIntersect xs1 xs2 (v1 :: acc)
    innerIntersect l1 l2 []
  
  static let union l1 l2 = 
    let rec innerUnion l1 l2 acc = 
      match (l1, l2) with
      | ([], []) -> List.rev acc
      | ([], l2) -> List.append (List.rev acc) l2
      | (l1, []) -> List.append (List.rev acc) l1
      | (x1 :: xs1, x2 :: xs2) -> 
        match (x1, x2) with
        | (v1, v2) when v1 > v2 -> innerUnion l1 xs2 (v2 :: acc)
        | (v1, v2) when v1 < v2 -> innerUnion xs1 l2 (v1 :: acc)
        | (v1, v2) -> innerUnion xs1 xs2 (v1 :: acc)
    innerUnion l1 l2 []
  
  static let deduplicate acc y = 
    match acc with
    | [] -> [ y ]
    | x :: xs -> 
      if x = y then acc
      else y :: acc
  
  member internal s.List = l
  member s.Remove(x) = remove x l []
  member s.Length() = List.length l
  member s.MinElement() = minimumElement l
  member s.Add(x) = union [ x ] l
  static member Empty : ListSet<'T> = empty
  static member Intersection(a : ListSet<'T>, b : ListSet<'T>) : ListSet<'T> = ListSet(intersect a.List b.List)
  static member Union(a : ListSet<'T>, b : ListSet<'T>) : ListSet<'T> = ListSet(union a.List b.List)
  static member (+) (a : ListSet<'T>, b : ListSet<'T>) : ListSet<'T> = ListSet(union a.List b.List)
  static member (*) (a : ListSet<'T>, b : ListSet<'T>) : ListSet<'T> = ListSet(intersect a.List b.List)
  new(elements : seq<'T>) = 
    let l = elements
           |> Seq.sort
           |> Seq.fold deduplicate []
           |> List.rev
    ListSet(l)

module ListSet = 
  let intersect a b = ListSet.Intersection(a, b)
  let union a b = ListSet.Union(a, b)
  let remove x (s : ListSet<'T>) = s.Remove(x)
  let ofSeq l = ListSet(l)
  let singleton x = ListSet([ x ])
  let minElement (s : ListSet<'T>) = s.MinElement()
  let length (s : ListSet<'T>) = s.Length()
  let add x (s : ListSet<'T>) = s.Add(x)
  let empty<'T when 'T : comparison> : ListSet<'T> = ListSet<'T>.Empty
