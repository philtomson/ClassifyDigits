
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module classifyDigits.Main
// This F# dojo is directly inspired by the 
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.
 
// The goal of the dojo will be to
// create a classifier that uses training data
// to recognize hand-written digits, and
// evaluate the quality of our classifier
// by looking at predictions on the validation data.

// Two data files are included in the same place you
// found this script: 
// trainingsample.csv, a file that contains 5,000 examples, and 
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.
 
// 1. GETTING SOME DATA
 
// First let's read the contents of "trainingsample.csv"

// We will need System and System.IO to work with files,
// let's right-click / run in interactive, 
// to have these namespaces loaded:
  
open System
open System.IO

type LabelPixels = { Label: int; Pixels: int[] }
let slurp_file file = 
   File.ReadAllLines(file).[1..] 
   |> Array.map (fun line -> line.Split(','))  
   |> Array.map (fun numline -> Array.map (fun (x:string) -> Convert.ToInt32(x)) numline)    
   |> Array.map (fun line -> { Label= line.[0]; Pixels=line.[1..] })
  
let trainingset = slurp_file("/home/phil/devel/f_sharp/Dojo-Digits-Recognizer/Dojo/trainingsample.csv") 
 
// 6. COMPUTING DISTANCES
 
// We need to compute the distance between images
// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))

//let distance (p1: int[]) (p2: int[]) = 
//  Math.Sqrt (float(Array.sum (Array.map2 ( fun a b -> (pown (a-b) 2)) p1 p2) ))
 
// distance implementation suggested by dfarmernv on reddit/r/fsharp 
// advantage: only makes one pass over array
let distance (p1: int[]) (p2: int[]) = 
     Array.fold2 (fun acc a b -> (acc + (a-b) * (a-b))) 0 p1 p2 |> float |> Math.Sqrt
    
 
// 7. WRITING THE CLASSIFIER FUNCTION
 
// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and predict
// the value of that closest element.
 
let classify (pixels: int[]) =
  //Array.map (fun x -> {Label= x.Label; Dist= (distance pixels x.Pixels )}) trainingset
  fst (trainingset |> Array.Parallel.map (fun x -> (x.Label, (distance pixels x.Pixels ) ))
                   |> Array.minBy (fun x -> snd x ))
 
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



let _ = 
    Console.WriteLine("start...")
    let validationsample = slurp_file("/home/phil/devel/f_sharp/Dojo-Digits-Recognizer/Dojo/validationsample.csv") 
    let num_correct = (validationsample |> Array.Parallel.map (fun p -> if (classify p.Pixels ) = p.Label then 1 else 0)
                                        |> Array.sum) 
    Printf.printf "Percentage correct:%f\n" ((float(num_correct)/ (float(Array.length validationsample)))*100.0)
