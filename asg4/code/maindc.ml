(* $Id: maindc.ml,v 1.4 2012-02-16 17:47:43-08 - - $ *)

include Scanner
include Bigint

open Bigint
open Str
open Printf
open Format
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let hash = Hashtbl.create 256

let store (thestack: stack_t) (reg: int) = 
    try let stack = Hashtbl.find hash reg
        in try let top = pop thestack
            in push top stack
        with Stack.Empty -> print_stackempty ()
    with Not_found ->
        try let top = pop thestack
            in let stack = Stack.create()
            in  Hashtbl.add hash reg stack
        with Stack.Empty -> print_stackempty ()

let load  (thestack: stack_t) (reg: int) =
    try let stack = Hashtbl.find hash reg
        in try let top = pop stack
            in push top thestack
        with Stack.Empty -> print_stackempty ();
    with Not_found ->  print_stackempty ();

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let wrap s postfix width =
  let width = width
  in let len   = String.length s
  in let rec loop start =
    if start >= len then []
    else
      let stop = min (len - start) width
      in let sub  = String.sub s start stop
      in let sub = match len with
        | len when len < 69 -> sub
        | len when (len - start) > 69 -> sub ^ postfix
        | len -> sub 
      in sub :: loop (start+stop)
  in
    loop 0

let wrap_and_print s postfix width =
  List.iter print_endline (wrap s postfix width)
  
let print_number number =
    let s = string_of_bigint number
    in wrap_and_print s "\\" 69
    
let print_stackempty () = eprintf "dc: stack empty\n%!"

(*let makeStack item = 
   Stack.create()
    in push item stack
    in stack*)
    

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> printf "not finished"
        | 's' -> printf "not finished"
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "%!";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
