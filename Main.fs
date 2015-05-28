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

module Program =
  open System
  open System.IO

  open SMO

  type KernelType = 
    | BOW
    | ST 
    | SST
    | PT 
    | SN 
    | LSN 
    | LASN

    static member toString (x: KernelType) = 
      match x with
      | BOW -> "BOW"
      | ST -> "ST"
      | SST -> "SST"
      | PT -> "PT"
      | SN -> "SN"
      | LSN -> "LSN"
      | LASN -> "LASN"

    static member ofString (s: string) = 
      match s with
      | "BOW" -> Some BOW
      | "ST" -> Some ST
      | "SST" -> Some SST
      | "PT" -> Some PT
      | "SN" -> Some SN
      | "LSN" -> Some LSN
      | "LASN" -> Some LASN
      | _ -> None

  let (|Kernel|_|) (s: string) = KernelType.ofString s

  let (|Double|_|) s =
    let mutable value = 0.0
    if System.Double.TryParse(s, &value) then Some value
    else None

  let (|Int|_|) s =
    let mutable value = 0
    if System.Int32.TryParse(s, &value) then Some value
    else None

  type TrainParams = {
    mutable Kernel: KernelType;
    mutable C: double;
    mutable RandomSeed: int32;
    mutable NumIter: int;
    mutable KernelParams: Map<string, string>;
  }

  let readlines (filename: string) = File.ReadLines(filename, Text.Encoding.UTF8)

  let saveModel (o: TrainParams) mA mY mX (path: string) =
    use fout = new IO.StreamWriter(path, false, Text.Encoding.UTF8) in
      let writen (s: string) = fout.WriteLine(s)
      let writefn (f: double) = fout.WriteLine(f)
      let writedn (d: int) = fout.WriteLine(d)
      writen "-k";
      writen (KernelType.toString o.Kernel);
      writen "-c";
      writefn o.C;
      writen "-s";
      writedn o.RandomSeed;
      writen "-i";
      writedn o.NumIter;
      writen "-p";
      let joiner s k v =
        if String.IsNullOrEmpty s then sprintf "%s=%s" k v
        else sprintf "%s;%s=%s" s k v
      writen (Map.fold joiner "" o.KernelParams);
      Seq.zip3 mA mY mX |> Seq.filter (fun (a, _, _) -> a > 0.0) |> Seq.map (fun (a, y, x) -> sprintf "%f %s" (a * double y) x) |> Seq.iter writen;

  let haltWith m =
    eprintfn "%s" m
    exit 1

  let findParam key map =
    let value = Map.tryFind key map
    match value with
    | None -> haltWith (sprintf "ERROR: cannot find kernel parameter '%s'" key)
    | Some x -> x

  let parseDouble key map =
    match findParam key map with 
    | Double v -> v
    | x -> haltWith (sprintf "ERROR: cannot parse double parameter '%s=%s'" key x)

  let parseInt key map =
    match findParam key map with 
    | Int v -> v
    | x -> haltWith (sprintf "ERROR: cannot parse double parameter '%s=%s'" key x)

  let parseKernelArgs (p: string) =
    if String.IsNullOrWhiteSpace p then Map.empty
    else
      let splitOne (s: string) =
        match s.Split [|'='|] with
        | [|k; v|] -> (k, v)
        | _ -> haltWith (sprintf "ERROR: cannot parse parameter: '%s'" s)
      p.Split [|';'|] |> Seq.map splitOne |> Map.ofSeq

  let rec parseTrainArgs args (o: TrainParams) =
    match args with
    | "-k" :: xs ->
      match xs with 
      | [] -> haltWith "ERROR: kernel is not specified properly"
      | Kernel k :: xss  -> parseTrainArgs xss {o with Kernel = k}
      | x :: _ -> haltWith (sprintf "ERROR: cannot recognize kernel: '%s'" x)
    | "-c" :: xs ->
      match xs with
      | [] -> haltWith "ERROR: C is not specified properly"
      | (Double value) :: xss -> parseTrainArgs xss {o with C = value}
      | x :: _ -> haltWith (sprintf "ERROR: cannot parse C: '%s'" x)
    | "-s" :: xs ->
      match xs with
      | [] -> haltWith "ERROR: random seed is not specified properly"
      | (Int value) :: xss -> parseTrainArgs xss {o with RandomSeed = value}
      | x :: _ -> haltWith (sprintf "ERROR: cannot parse random seed: '%s'" x)
    | "-p" :: xs ->
      match xs with
      | [] -> haltWith "ERROR: kernel parameters are not specified"
      | x :: xss -> parseTrainArgs xss {o with KernelParams = parseKernelArgs x}
    | "-i" :: xs ->
      match xs with
      | [] -> haltWith "ERROR: # of iterations is not specified"
      | (Int value) :: xss -> parseTrainArgs xss {o with NumIter = value}
      | x :: _ -> haltWith (sprintf "ERROR: cannot parse # of iterations: '%s'" x)
    | tail -> (o, tail)

  let loadModel (path: string) = 
    use fin = new IO.StreamReader(path, Text.Encoding.UTF8) in
      let readline() = fin.ReadLine()
      let args = seq { for i = 1 to 10 do yield readline() } |> Seq.toList
      let defs = {
        Kernel = BOW;
        C = 1.0;
        RandomSeed = int32 (System.DateTime.Now.Ticks);
        NumIter = 1 <<< 24;
        KernelParams = Map.empty;
      }
      let options, _ = parseTrainArgs args defs
      let folder xs (l: string) =
        let l = l.Trim()
        let index = l.IndexOf " "
        let yalpha = l.Substring(0, index)
        let tail = l.Substring(index + 1).Trim()
        match yalpha with
        | Double v -> (v, tail) :: xs
        | _ -> haltWith (sprintf "ERROR: cannot parse support vector: %s" l)
      let lines = Seq.initInfinite (fun _ -> readline()) |> Seq.takeWhile (fun line -> line <> null)
      let sv = lines |> Seq.fold folder []
      (options, sv)

  let addDefualtKernelParams o =
    let addIfNotPresent map (k,v) =
      if Map.containsKey k map then map
      else Map.add k v map
    let p =
      match o.Kernel with 
      | BOW -> [] 
      | ST | SST -> [("lambda", "0.4")] 
      | PT -> [("lambda", "0.4"); ("mu", "0.4")]
      | SN | LSN | LASN -> [("lambda", "0.4"); ("maxdist", "3"); ("maxsize", "3")]
    {o with KernelParams = p |> List.fold addIfNotPresent o.KernelParams}

  let (|BinaryLabeledInstance|_|) (s: string) =
    if s.Length < 3 then None
    else 
      match s.Substring(0, 2) with
      | "+1" -> Some (1, s.Substring(3).Trim())
      | "-1" -> Some (-1, s.Substring(3).Trim())
      | _ -> None

  let readData path =
    let folder ((ys, xs) as acc) (l: string) =
      match l.Trim() with
      | BinaryLabeledInstance (y, x) -> (y::ys, x::xs)
      | _ -> acc

    let rev (ys, xs) = (List.rev ys, List.rev xs)

    path |> readlines |> Seq.fold folder ([], []) |> rev

  let train args = 
    let defs = {
      Kernel = BOW;
      C = 1.0;
      RandomSeed = int32 (System.DateTime.Now.Ticks);
      NumIter = 1 <<< 24;
      KernelParams = Map.empty;
    }

    let options, xs = parseTrainArgs args defs
    let options = addDefualtKernelParams options

    let trainingFile, modelFile =
      match xs with 
      | x :: y :: [] -> (x, y)
      | _ -> haltWith "ERROR: free args are not specified"

    let yList, xList = readData trainingFile
    let y = yList |> List.toArray
    let n = Array.length y
    let kernelCache = Array2D.init n n (fun _ _ -> -1.0)

    let inline xBuilder repr = xList |> List.map repr |> List.toArray
    let inline kernelBuilder (x: _ []) kernel = Kernel.normCachedIJKernel (fun i j -> kernel x.[i] x.[j]) kernelCache

    let inline SMO_WSS3_WITH_TASK x = SMO_WSS3 {TaskParams.C = options.C; TaskParams.NumIter = options.NumIter;} {IJKernel = x; y = y;}
    let inline optimize xx kk = SMO_WSS3_WITH_TASK (kernelBuilder (xBuilder xx) kk)

    let findKernelParam (k: string) parser = parser k options.KernelParams

    let A =
      match options.Kernel with
      | BOW -> 
        optimize InterTree.ofString InterTree.inter_kernel
      | ST -> 
        let lambda = findKernelParam "lambda" parseDouble
        optimize SubSetTree.ofString (SubSetTree.st_kernel lambda)
      | SST ->
        let lambda = findKernelParam "lambda" parseDouble
        optimize SubSetTree.ofString (SubSetTree.sst_kernel lambda)
      | PT ->
        let lambda = findKernelParam "lambda" parseDouble
        let mu = findKernelParam "mu" parseDouble
        optimize PartialTree.ofString (PartialTree.pt_kernel lambda mu)
      | SN ->
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        optimize SkipNodeTree.ofString (SkipNodeTree.sn_kernel lambda maxDist maxSize)
      | LSN ->
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        optimize SkipNodeTree.ofString (SkipNodeTree.lsn_kernel lambda maxDist maxSize)
      | LASN ->       
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        optimize SkipNodeTree.ofString (SkipNodeTree.lasn_kernel lambda maxDist maxSize)

    saveModel options A y xList modelFile

  let classify args = 
    let testFile, modelFile, outFile =
      match args with
      | x :: y :: z :: [] -> (x, y, z)
      | _ -> haltWith "ERROR: free args are not specified"
    
    let options, alphaI = loadModel modelFile 

    let parseLine (l: string) =
      match l.Trim() with
      | BinaryLabeledInstance i -> i
      | _ -> haltWith "ERROR: cannot parse test file"

    let examples = readlines testFile |> Seq.map parseLine 

    let inline predict repr kernel =
      let alphaSV = alphaI |> List.map (fun (a, i) -> (a, repr i)) |> List.toArray
      let sqrtKsv = alphaSV |> Array.map (fun (_, sv) -> sqrt (kernel sv sv))
      let n = alphaSV.Length

      let result = seq {
        for ex in examples do
          let reprEx = repr (snd ex)
          let sqrtKernelEx = sqrt (kernel reprEx reprEx)
          let sum = 
            let mutable value = 0.0
            for i = 0 to n - 1 do
              let alpha, sv = alphaSV.[i]
              let sqrtKsv = sqrtKsv.[i]
              value <- value + alpha * (kernel sv reprEx) / (sqrtKsv * sqrtKernelEx)
            value
          yield (fst ex, sum)
      }
      result

    let findKernelParam (k: string) parser = parser k options.KernelParams

    let goldAndPrediction =
      match options.Kernel with
      | BOW -> 
        predict InterTree.ofString InterTree.inter_kernel
      | ST -> 
        let lambda = findKernelParam "lambda" parseDouble
        predict SubSetTree.ofString (SubSetTree.st_kernel lambda)
      | SST ->
        let lambda = findKernelParam "lambda" parseDouble
        predict SubSetTree.ofString (SubSetTree.sst_kernel lambda)
      | PT ->
        let lambda = findKernelParam "lambda" parseDouble
        let mu = findKernelParam "mu" parseDouble
        predict PartialTree.ofString (PartialTree.pt_kernel lambda mu)
      | SN ->
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        predict SkipNodeTree.ofString (SkipNodeTree.sn_kernel lambda maxDist maxSize)
      | LSN ->
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        predict SkipNodeTree.ofString (SkipNodeTree.lsn_kernel lambda maxDist maxSize)
      | LASN ->       
        let lambda = findKernelParam "lambda" parseDouble
        let maxDist = findKernelParam "maxdist" parseInt
        let maxSize = findKernelParam "maxsize" parseInt
        predict SkipNodeTree.ofString (SkipNodeTree.lasn_kernel lambda maxDist maxSize)

    let tp = ref 0
    let tn = ref 0
    let fp = ref 0
    let fn = ref 0

    use fout = new IO.StreamWriter(outFile, false, Text.Encoding.UTF8) in
      let writefn (f: double) = fout.WriteLine(f)

      let procPred (g, p) =
        writefn(p);
        if g > 0 && p > 0.0 then tp := !tp + 1;
        if g > 0 && p < 0.0 then fn := !fn + 1;
        if g < 0 && p > 0.0 then fp := !fp + 1;
        if g < 0 && p < 0.0 then tn := !tn + 1;

      goldAndPrediction |> Seq.iter procPred
      
    let a = (double (!tp + !tn)) / (double (!tp + !tn + !fp + !fn))
    let p = (double (!tp)) / (double (!tp + !fp))
    let r = (double (!tp)) / (double (!tp + !fn))
    let f = 2.0 * p * r / (p + r)

    printfn "Accuracy     = %g" (a * 100.0)
    printfn "Precision(+) = %g" (p * 100.0)
    printfn "Recall(+)    = %g" (r * 100.0)
    printfn "F-measure(+) = %g" (f * 100.0)

  let printUsage () =
    printfn "Tree-SVM: tree kernels for SVM"
    printfn ""
    printfn "usage: tree-svm train [options] example_file model_file"
    printfn "usage: tree-svm classify [options] example_file model_file output_file"
    printfn "usage: tree-svm help"
    printfn ""

  let printHelp () =
    printUsage ()
    printfn "Arguments:"
    printfn "    example_file -> file with traning/testing data"
    printfn "    model_file   -> file with learned model"
    printfn "    output_file  -> file with classification output"
    printfn ""
    printfn "Learning options:"
    printfn "    -c double    -> trade-off between a large margin and a small error penalty"
    printfn ""
    printfn "    -i int       -> maximum number of interations"
    printfn ""
    printfn "    -k string    -> type of kernel function:"
    printfn "                      BOW  -> number of common labels (bag-of-words analog for trees)"
    printfn "                              params: -"
    printfn ""
    printfn "                      ST   -> subtree kernel"
    printfn "                              params: lambda"
    printfn ""
    printfn "                      SST  -> subset tree kernel"
    printfn "                              params: lambda"
    printfn ""
    printfn "                      PT   -> partial tree kernel"
    printfn "                              params: lambda, mu"
    printfn ""
    printfn "                      SN   -> skip-node kernel"
    printfn "                              params: lambda, maxdist, maxsize"
    printfn ""
    printfn "                      LSN  -> linear skip-node kernel"
    printfn "                              params: lambda, maxdist, maxsize"
    printfn ""
    printfn "                      LASN -> lookahead skip-node kernel"
    printfn "                              params: lambda, maxdist, maxsize"
    printfn ""
    printfn "    -p string    -> kernel parameter string (e.g. \"lambda=0.4;mu=0.5\")"
    printfn "                      lambda  -> decay factor in tree kernel"
    printfn "                      mu      -> mu decay factor in partial tree kernel"
    printfn "                      maxdist -> maximal size of a gap in edges (in skip-node kernels)"
    printfn "                      maxsize -> maximal size of a substructure (in skip-node kernels)"
    printfn ""
    printfn "Input data format example:"
    printfn "+1 (beats (clarity (on (#CAMERA))) (out) (easily) (clarity (on (#CAMERA))))"
    printfn "-1 (#CAMERA (is (2.5 (same (as (that (on (#CAMERA))))) (wide))))"
    printfn ""

  let run args =
    match args with
    | [] -> 
      printUsage ()
      haltWith "ERROR: action is not specified"
    | "train" :: xs -> train xs
    | "classify" :: xs -> classify xs
    | "-h" :: xs | "--help" :: xs | "help" :: xs -> 
      printHelp ()
    | x :: xs -> 
      printUsage ()
      haltWith (sprintf "ERROR: wrong action: '%s'" x)

  [<EntryPoint>]
  let main argv = 
    run (Array.toList argv)
    0
