open System
open System.Net
open System.Threading

/////////////////////////////////////////
// Computation Expressions

[<AbstractClass>]
type M<'a> =
    abstract Bind : M<'a> -> ('a -> M<'b>) -> M<'b>
    abstract Return : 'a -> M<'a>
    abstract ReturnFrom : M<'a> -> M<'a>

///////////////////

let oReturn (x: 'a) : 'a option = Some(x)

let oReturnFrom (x: 'a option) : 'a option = x

let oBind (mx: 'a option) (f: 'a -> 'b option) : 'b option = 
    match mx with
    | Some(x) -> f x
    | None -> None
                                                             
let (>>=) = oBind

type OptionMonad() =
    member x.Bind(p, f) = oBind p f
    member x.Return(y) = oReturn y
    member x.ReturnFrom(y) = oReturnFrom y

let option = new OptionMonad()

let isEven x = if x % 2 = 0 then Some x else None 
let minus2 x = Some(x - 2)
let div10 x = if x = 0 then None else Some(10 / x)

Some(4) >>= isEven >>= minus2 >>= div10
Some(5) >>= isEven >>= minus2 >>= div10
Some(2) >>= isEven >>= minus2 >>= div10

let a = option {
    let! a = Some(2)
    let! b = isEven a
    let! c = minus2 b
    let! d = div10 c
    return d
}
let a = option {
    let! a = Some(4)
    let! b = isEven a
    let c = b - 2
    let! d = div10 c
    return d
}


option { 
    let! x = Some 1
    let! y = Some 2
    return x + y
} |> printfn "Result 2: %A" 


option.Bind (Some 1, fun x ->
    option.Bind (Some 2, fun y ->
        option.Return (x+y)
    )
) |> printfn "Result 2: %A"
 

let lReturn (x: 'a) : 'a list = ?

let lReturnFrom (x: 'a list) : 'a list = ?

let lBind (mx: 'a list) (f: 'a -> 'b list) : 'b list = ?

let (>>=) = lBind

type ListMonad() =
    member x.Bind(p, f) = lBind p f
    member x.Return(y) = lReturn y
    member x.ReturnFrom(y) = lReturnFrom y

let list = new ListMonad()

[1;2;3] >>= (fun x -> [x; x+1; x+2])

let a = list {
    let! a = [1;2;3]
    let c = 13
    let! b = [4;5;6]
    printf "%A-%A-%A\n" a b c
    return a
    // return! [a;a+1;a+2]
} 

// writer

let wReturn (x: 'a) : ('a * string) = (x, " got " + x.ToString() + ".")
let wReturnFrom (x: 'a * string) : 'a * string = x
let wBind (mx: ('a * string)) (f: 'a -> 'b * string) : 'b * string = 
    let res = f (fst mx)
    (fst res, snd mx + snd res)
                                                       
let (>>=) = wBind

type WriterMonad() =
    member x.Bind(p, f) = wBind p f
    member x.Return(y) = wReturn y
    member x.ReturnFrom(y) = wReturnFrom y

let writer = new WriterMonad()

let squared x = (x * x, " was squared.")
let halved x = (x / x, " was halved.")
 
let a = writer {
    let! a = wReturn 4
    let! b = squared a
    let! c = halved a
    return c
} // (1, " got 4. was squared. was halved. got 1.")

//////////////////////////////////////////////////////////////

open System
open System.Threading

let ts() = System.DateTime.Now.Ticks

let waitSync id time =
    printfn "%d start" id
    let ts1 = ts()
    Thread.Sleep(time * 1000)
    let ts2 = ts()
    let delta = System.TimeSpan(ts2 - ts1)
    printfn "%d end %s" id (delta.ToString())
    13

[1..10]
  |> List.mapi (fun index time -> waitSync index 1)

let wait id time =
    async {
       printfn "%d start" id
       let ts1 = ts()
       do! Async.Sleep(time * 1000)
       let ts2 = ts()
       let delta = System.TimeSpan(ts2 - ts1)
       printfn "%d end %s" id (delta.ToString("G"))
       return 13
    }

Async.RunSynchronously (wait 1 1)

[1..10]
  |> List.map (fun index -> Async.RunSynchronously (wait index 1))

[1..10]
  |> List.map (fun index -> wait index 1)
  |> Async.Parallel
  |> Async.RunSynchronously

let inside = async {
    let! a = wait 1 1
    let! b = wait 2 1
    return a + b
}
Async.RunSynchronously inside

let inside = async {
    let a = wait 1 1
    let b = wait 2 1
    let [|c; d|] = [a; b] |> Async.Parallel |> Async.RunSynchronously
    return c + d
}
Async.RunSynchronously inside

let inside = async {
    let a = wait 1 1
    let b = wait 2 1
    return! [a; b] |> Async.Parallel
}
Async.RunSynchronously inside

module Async =
  let fmap f workflow = async {
    let! res = workflow
    return f res
  }
  let map f workflow = List.map (fmap f) workflow

[1..10]
  |> List.map (fun index -> wait index 1)
  //|> List.map (Async.fmap (fun x -> x * 2))
  |> Async.map (fun x -> x * 2)
  |> Async.Parallel
  |> Async.RunSynchronously


//////////////////////////////////////////////////

open System.Net
let req1 = HttpWebRequest.Create("http://yandex.ru")
let req2 = HttpWebRequest.Create("http://google.com")
let req3 = HttpWebRequest.Create("http://bing.com")
req1.BeginGetResponse((fun r1 -> 
    use res1 = req1.EndGetResponse(r1)
    printfn "Downloaded %O" res1.ResponseUri
    req2.BeginGetResponse((fun r2 -> 
        use res2 = req2.EndGetResponse(r2)
        printfn "Downloaded %O" res2.ResponseUri
        req3.BeginGetResponse((fun r3 -> 
            use res3 = req3.EndGetResponse(r3)
            printfn "Downloaded %O" res3.ResponseUri
            ),null) |> ignore
        ),null) |> ignore
    ),null) |> ignore

open System.Net
let req1 = HttpWebRequest.Create("http://yandex.ru")
let req2 = HttpWebRequest.Create("http://google.com")
let req3 = HttpWebRequest.Create("http://bing.com")

async {
    use! res1 = req1.AsyncGetResponse()  
    printfn "Downloaded %O" res1.ResponseUri
    use! res2 = req2.AsyncGetResponse()  
    printfn "Downloaded %O" res2.ResponseUri
    use! res3 = req3.AsyncGetResponse()  
    printfn "Downloaded %O" res3.ResponseUri
    } |> Async.RunSynchronously

let downloadPage (url: string) = async {
    let req = HttpWebRequest.Create(url)
    use! res = req.AsyncGetResponse()
    printfn "Downloaded %O" res.ResponseUri
}

["http://yandex.ru"; "http://google.com"; "http://bing.com"]
    |> List.map downloadPage
    |> Async.Parallel
    |> Async.RunSynchronously

#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data

let downloadPage (url: string) = async {
    let! html = Http.AsyncRequestString(url)
    return url, html.Length
}

["http://yandex.ru"; "http://google.com"; "http://bing.com"]
    |> List.map downloadPage
    |> Async.Parallel
    |> Async.RunSynchronously

open System.IO
open System.Text
// let bases = HtmlDocument.Load("http://mipt.ru/diht/bases/")
let html = File.ReadAllText("/home/und/fsharp/bases.html")
let bases = HtmlDocument.Parse(html)
bases.Descendants ["td"]  
    |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
    // для получения ссылок вместо InnerText нужно использовать методы TryGetAttribute, Attibute или AttributeValue
    // см. исходный код https://github.com/fsharp/FSharp.Data/blob/master/src/Html/HtmlOperations.fs
    |> Seq.map (fun x -> x.InnerText()) 
    |> Seq.toList
    
    
    
    
/////////////////////////////////////////////////////////////////

let rec fib x = if x <= 2I then 1I else fib(x-1I) + fib(x-2I)

fib 10I

let duration f = 
  let sw = new System.Diagnostics.Stopwatch()
  sw.Start()
  let v = f()
  sw.Stop()
  printf "%A\n" sw.ElapsedMilliseconds
  v
  
// одно ядро
duration(fun() -> 
  [for i in 0..3 -> fib 32I]
  |> ignore
  )
// много ядер
let fibA x = async {
  return fib x
}

duration (fun () ->
  [ for i in 0..3 -> fibA 32I ]
  |> Async.Parallel 
  |> Async.RunSynchronously
  |> ignore
  )

let countSAS x =
  printf "%A\t" x
  duration(fun() -> 
    [for i in 0..3 -> fib x]
    |> ignore
    )
  printf "\t"
  duration (fun () ->
    [ for i in 0..3 -> fibA x]
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> ignore
  )
  printf "\n"

countSAS 30I

for i in [25I..30I] do countSAS i
(*
30  4112L     1040L
31  6697L     1657L
32  10651L  2616L
33  17321L  4204L
34  28179L  6777L
35  45293L  10971L
36  72539L  17768L
*)

// загрузка 4-х из 8-ми ядер
let fibs2 =
  [| for i in 0..3 do yield! [|1I; 34I|] |]
  |> Array.map (fun x -> fibA x)
  |> Async.Parallel
  |> Async.RunSynchronously

// своя реализация map
let pmap f l =
  seq {for i in l -> async {return f i}}
  |> Async.Parallel
  |> Async.RunSynchronously

let rec fibs x = if x <= 2 then 1 else fibs(x-1) + fibs(x-2)

// основана на PLINQ
// предыдущая - создаёт 100500 заданий, а потом стартует
#r "../packages/FSharp.Collections.ParallelSeq.1.0.2/lib/net40/FSharp.Collections.ParallelSeq.dll"
open FSharp.Collections.ParallelSeq

// большие числа
duration (fun() -> pmap fibs [40..44]) |> printf "\nFibsAsync: %A"
duration (fun() -> List.map fibs [40..44]) |> printf "\nFibsSync: %A"
duration (fun() -> [40..44] |> PSeq.map fibs |> PSeq.toList) |> printf "\nFibsPsync: %A"
// много маленьких чисел
let manymany = List.init 30000 (fun _ -> 5)
duration (fun() -> pmap fibs manymany) |> printf "\nFibsAsync: %A"
duration (fun() -> List.map fibs manymany) |> printf "\nFibsSync: %A"
duration (fun() -> manymany |> PSeq.map fibs |> PSeq.toList) |> printf "\nFibsPsync: %A"



[1..10000] |> List.filter (fun n -> List.filter (fun x -> n%x=0) [1..n/2] |> List.sum = n)

//6 = 1+2+3
//28 = 1+2+4+7+14


let dividers n = List.filter (fun x -> n%x=0) [1..n/2]
let perfect n = dividers n |> List.sum = n
duration (fun _ -> [1..10000] |> List.filter perfect) |> printf "\n%A"  // 2700
//System.Int16.MaxValue
duration (fun() -> [1..35000000] |> List.filter perfect) |> printf "%A" // undefined

let pdividers n = seq {1..n/2} |> PSeq.filter (fun x -> n%x=0)
let pperfect n = pdividers n |> PSeq.sum = n
duration (fun _ -> [1..10000] |> PSeq.filter pperfect |> PSeq.toList) |> printf "\n%A" // 1700
duration (fun _ -> [1..35000000] |> PSeq.filter pperfect |> PSeq.toList) |> printf "\n%A" // undefined


// не всё можно решить параллельностью

let sqrtInt (n:int) = (int)(System.Math.Sqrt((float)n))

let testDiv n acc x =
  if (n%x=0) then 
    let y = n/x
    if (x=y) then x::acc else x::y::acc
  else acc

let dividers2 n = (List.fold (fun a x -> testDiv n a x) [1] [2..(sqrtInt n)])
let perfect2 n = dividers2 n |> List.sum = n
duration (fun _ -> [2..10000] |> List.filter perfect2) |> printf "\n%A"  // 90
duration (fun _ -> [2..35000000] |> List.filter perfect2) |> printf "\n%A"// всю ночь, UNDEFINED

// декларативный язык прячет реализацию

let pdividers2 n = seq {2..(sqrtInt n)} |> PSeq.fold (fun a x -> testDiv n a x) [1]
let pperfect2 n =
  if (n%10000=0) then printfn "%A" n
  pdividers2 n |> PSeq.sum = n

duration (fun _ -> [2..10000] |> PSeq.filter pperfect2 |> PSeq.toList) |> printf "\n%A" // 200
duration (fun _ -> [2..35000000] |> PSeq.filter pperfect2 |> PSeq.toList) |> printf "\n%A" // полтора часа

