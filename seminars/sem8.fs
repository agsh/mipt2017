type 'a Tree =
  EmptyTree
  | Node of 'a * 'a Tree * 'a Tree

let singleton x = Node (x, EmptyTree, EmptyTree)

let rec treeInsert x = function
  EmptyTree -> singleton x 
  | Node (a, left, right) -> 
    if x = a then Node (x, left, right) 
    else 
      if x < a then Node (a, (treeInsert x left), right) 
      else Node (a, left, (treeInsert x right))
// when 'a : comparsion

let list2tree list =
 let rec l2t acc = function
   [] -> acc
   | (head::tail) -> l2t (treeInsert head acc) tail
 in l2t EmptyTree list

// list2tree через fold

list2tree [12; 1; 6; 4; 90; 9]

tree2list // сами


let treesort x = x |> list2tree |> tree2list

treesort [12; 1; 6; 4; 90; 9]

list2tree [12; 12; 12; 13; 13; 14]
Как будет выглядеть дерево?

type 'a Tree =
  EmptyTree
  | Node of 'a * int * 'a Tree * 'a Tree

let singleton x = ? 

let rec treeInsert x = ?

let list2tree x = 
  let rec l2t acc = function
    | [] -> acc
    | (head::tail) -> l2t (treeInsert head acc) tail
l2t EmptyTree x

let foldTree treeFunction listValue tree =
    let rec loop tree cont =
        match tree with
        | EmptyTree -> cont listValue
        | Node (x, left, right) -> loop left (fun leftAcc -> 
            loop right (fun rightAcc -> 
              cont (treeFunction x leftAcc rightAcc)
            )
          )
    loop tree (fun x -> x)
    
// написать foldTree без продолжений


let sumTree = foldTree (fun x left right -> x + left + right) 0
[2;7;4;3;5;8] |> list2tree |> sumTree

let heightTree = foldTree (fun _ left right -> 1 + max left right) 0
[2;7;4;3;5;8] |> list2tree |> heightTree 

let tree2List = foldTree (fun x left right -> left @ (x :: right)) []
[2;7;4;3;5;8] |> list2tree |> tree2List

// найти максимальное значение в дереве

// перевернуть дерево

open System

let generate = 
  let rnd = Random()
  rnd.Next(42)

generate 0 42

// сгенерировать дерево

// проверить, что два дерева подобны

type 'a Tree =
  | EmptyTree
  | Leaf of 'a
  | Node of 'a * 'a Tree list

Node(1, [Leaf 3; EmptyTree; Node(2, [])])

// найти высоту дерева, EmptyTree не считаются

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) : 'r = 
    let recurse = fold fLeaf fNode  
    match tree with
    | LeafNode leafInfo -> 
        fLeaf acc leafInfo 
    | InternalNode (nodeInfo, subtrees) -> 
        Seq.fold recurse (fNode acc nodeInfo) subtrees 

// map через fold?

let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = ?

type FileInfo = {name:string; fileSize:int}
type DirectoryInfo = {name:string; dirSize:int}

type FileSystemItem = Tree<FileInfo,DirectoryInfo>

let fromFile (fileInfo:FileInfo) = 
    LeafNode fileInfo 

let fromDir (dirInfo:DirectoryInfo) subitems = 
    InternalNode (dirInfo,subitems)

let readme = fromFile {name="readme.txt"; fileSize=1}
let config = fromFile {name="config.json"; fileSize=2}
let build  = fromFile {name="build.sh"; fileSize=3}
let src = fromDir {name="src"; dirSize=10} [readme; config; build]
let bin = fromDir {name="bin"; dirSize=10} []
let root = fromDir {name="root"; dirSize=5} [src; bin]

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.fileSize
    let fDir acc (dir:DirectoryInfo)= 
        acc + dir.dirSize
    fold fFile fDir 0 fileSystemItem 

readme |> totalSize  
src |> totalSize     
root |> totalSize    

// largestFile : fileSystemItem:Tree<FileInfo,'a> -> FileInfo option
let largestFile fileSystemItem =

readme |> largestFile
src |> largestFile
bin |> largestFile
root |> largestFile

open System
open System.IO

DirectoryInfo("/home/und/fsharp")

type FileSystemTree = Tree<FileInfo,DirectoryInfo>

let fromFile (fileInfo:FileInfo) = 
    LeafNode fileInfo 

let rec fromDir (dirInfo:DirectoryInfo) = 
    let subItems = seq {
        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
    }
    InternalNode (dirInfo,subItems)

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.Length
    let fDir acc (dir:DirectoryInfo)= 
        acc 
    fold fFile fDir 0L fileSystemItem 
   
let currentDir = fromDir (DirectoryInfo("/home/und/fsharp"))

currentDir |> totalSize  

let largestFile fileSystemItem =
    let fFile (largestSoFarOpt:FileInfo option) (file:FileInfo) = 
        match largestSoFarOpt with
        | None -> 
            Some file                
        | Some largestSoFar -> 
            if largestSoFar.Length > file.Length then
                Some largestSoFar
            else
                Some file

    let fDir largestSoFarOpt dirInfo = 
        largestSoFarOpt

    fold fFile fDir None fileSystemItem

currentDir |> largestFile  

let dirListing fileSystemItem =
    let printDate (d:DateTime) = d.ToString()
    let mapFile (fi:FileInfo) = 
        sprintf "%10i  %s  %-s"  fi.Length (printDate fi.LastWriteTime) fi.Name
    let mapDir (di:DirectoryInfo) = 
        di.FullName 
    map mapFile mapDir fileSystemItem

currentDir 
    |> dirListing 
    |> map (printfn "%s") (printfn "\n%s")


// filter fs, fsx files
