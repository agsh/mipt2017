open System

let generate min max = 
  let rnd = Random()
  rnd.Next(min, max)

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

let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = ?

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



open System.Windows.Forms
open System.Drawing

let tree = s |> explode |> tokenize |> parse

let rec treeLoop (node:JSON) : TreeNode =
  match node with
    | JSON.Number i -> new TreeNode(i.ToString())
    | JSON.Array list ->
      let root = new TreeNode("[]")
      List.iter (fun v -> 
          root.Nodes.Add(treeLoop v) |> ignore
        ) list
      root
    | JSON.Object list ->
      let root = new TreeNode("{}")
      List.iter (fun (k, v) -> 
          let r = root.Nodes.Add(k.ToString())
          r.Nodes.Add(treeLoop v) |> ignore
        ) list
      root

let form = new Form(Height=400, Width=250, Text="JSON show", StartPosition=FormStartPosition.CenterScreen, MinimizeBox=false, ShowIcon=false)

let treeView = new TreeView(Dock = DockStyle.Fill)
treeView.Nodes.Add(treeLoop tree) |> ignore
treeView.ExpandAll()
form.Controls.Add(treeView)

form.Show()
Application.Run()
