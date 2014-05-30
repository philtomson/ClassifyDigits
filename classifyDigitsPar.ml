(* OCaml version using Parmap
   compile with:

ocamlfind ocamlopt -linkpkg -o classifyDigitsPar -package parmap -package str classifyDigitsPar.ml

*)

(*
// This F# dojo is directly inspired by the 
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.
 
*)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
  
(*
// Two data files are included in the same place you
// found this script: 
// trainingsample.csv, a file that contains 5,000 examples, and 
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.
 
// 1. GETTING SOME DATA
 
// First let's read the contents of "trainingsample.csv"
*)

type labelPixels = { label: int; pixels: int list }
let slurp_file file = 
   List.tl (read_lines file) 
   |> List.map (fun line -> Str.split (Str.regexp ",") line )  
   |> List.map (fun numline -> List.map (fun (x:string) -> int_of_string x) numline)    
   |> List.map (fun line -> { label= (List.hd line); pixels=(List.tl line) })
  
  
let trainingset = slurp_file("./trainingsample.csv") 

(*
 
// 6. COMPUTING DISTANCES
 
// We need to compute the distance between images
*)
let list_sum lst = List.fold_left (fun x acc -> x+.acc) 0.0 lst

let distance (p1: int list) (p2: int list) = 
  sqrt (list_sum (List.map2 ( fun a b -> (float_of_int(a-b)**2.0)) p1 p2) )
 
(* 
// 7. WRITING THE CLASSIFIER FUNCTION
 
// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and predict
// the value of that closest element.
*)

let minBy compf lst = 
  (List.fast_sort compf lst) |> List.hd

let classify (pixels: int list) =
  let label_dist = Parmap.parmap ~chunksize:50 ~ncores:4 (fun x -> (x.label, (distance pixels x.pixels) )) (Parmap.L trainingset) in
  let min = minBy (fun x y -> compare (snd x) (snd y)) label_dist in
  fst min
 
 
(*
// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA
 
// Now that we have a classifier, we need to check
// how good it is. 
// This is where the 2nd file, validationsample.csv,
// comes in handy. 
// For each Example in the 2nd file,
// we know what the true Label is, so we can compare
// that value with what the classifier says.
// You could now check for each 500 example in that file
// whether your classifier returns the correct answer,
// and compute the % correctly predicted.
*)

let validationsample = slurp_file("./validationsample.csv") 
let num_correct = 
  (Parmap.L validationsample) |> Parmap.parmap ~ncores:4 (fun p -> if (classify p.pixels ) = p.label then 1. else 0.) |> list_sum 

let _ = Printf.printf "Percentage correct:%f\n" (((num_correct)/. (float_of_int(List.length validationsample)))*.100.0)
