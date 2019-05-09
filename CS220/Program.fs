type Stream<'a> =
  | Nil
  | Cons of 'a * (unit -> Stream<'a>)

let car = function
  | Nil -> failwith "XXX"
  | Cons (hd, _) -> hd

let cdr = function
  | Nil -> failwith "XXX"
  | Cons (_, thunk) -> thunk ()

let rec take stream n =
  if n <= 0 then Nil
  else Cons (car stream,
             fun () -> take (cdr stream) (n - 1))

let fromList lst =
  List.rev lst
  |> List.fold (fun stream elt ->
                 Cons (elt, fun () -> stream)) Nil

let rec iter fn = function
  | Nil -> ()
  | Cons (hd, thunk) ->
    fn hd
    iter fn (thunk ())

let print stream n =
  let stream' = take stream n
  iter (printfn "%A") stream'

let rec ones =
  Cons (1, fun () -> ones)

let posint =
  let rec stream n =
    Cons (n, fun () -> stream (n + 1))
  stream 1

[<EntryPoint>]
let main argv =
  print posint 10
  0 // return an integer exit code
