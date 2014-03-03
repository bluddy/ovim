open Batteries
open Util

module IntMap = Map.Make(struct type t=int let compare a b = a-b end)

type loc = int * int (* line * char *)

type contents = {
  data : char array;
  new_lines : int array;
}

let num_lines_of_contents c = Array.length c.new_lines
let content_length c = Array.length c.data

exception Stop of int

(* count lines between a range of chars using the array *)
(* TODO: find first with binary search *)
let count_lines contents first last =
  try
    Array.fold_left (fun acc x ->
      if x > last   then raise @@ Stop(acc) else
      if x >= first then acc + 1 else acc
    ) 0 contents.new_lines
  with Stop x -> x

(* a sub-buffer. starts and ends at specific characters *)
type sub = {
  contents : contents;
  first : int;
  last :  int;
  num_lines : int;
}

let sub_length s = s.last - s.first + 1

type chunk =
  | Content of contents
  | Sub of sub

let chunk_lines = function
  | Content c -> num_lines_of_contents c
  | Sub s     -> s.num_lines

let chunk_length = function
  | Content c -> content_length c
  | Sub s     -> sub_length s

type buffer = {
  file_name : string option;
  chunks : chunk list;
}

let chunk_of_string s =
  let _, lines, acc = 
    String.fold_left (fun (index, line_l, acc) -> function
      | '\n' as c -> index+1, index::line_l, c::acc
      | c         -> index+1, line_l,        c::acc
    ) (0, [], []) s
  in
  let data = Array.of_list @@ List.rev acc in
  let new_lines = Array.of_list @@ List.rev lines in
  Content {data; new_lines}

let buffer_of_file fname s = 
  let chunks = [chunk_of_string s] in
  {file_name=Some fname; chunks}

let load_buffer fname = 
  let s = File.with_file_in fname IO.read_all in
  buffer_of_file fname s

let find_chunk buffer loc =
  foldl_until (fun count chunk ->
    let c = chunk_chars chunk in
    if c >= count then Left chunk
    else Right(count - c)
  ) loc buffer.chunks

let split_chunk loc = function
  | Sub s when sub_length s < loc
          -> failwith "Out of bounds in sub"
  | Sub s ->
      let last = s.first + loc in
      let num_lines = count_lines s.contents s.first last in
      let s1 = Sub {s with last; num_lines} in
      let first = last + 1 in
      let num_lines = count_lines s.contents first s.last in
      let s2 = Sub {s with first; num_lines} in
      s1, s2
  | Content c when content_length c <= loc
         -> failwith "Out of bounds in content"
  | Content c ->
      let num_lines = count_lines c 0 loc in
      let s1 = Sub {contents=c; num_lines; first=0; last=loc} in
      let last = content_length c - 1 in
      let first = loc + 1 in
      let num_lines = count_lines c first last in
      let s2 = Sub {contents=c; num_lines; first; last} in
      s1, s2
  
let append_string buffer s =
  let cs = chunk_of_string s in
  {buffer with chunks = buffer.chunks@[cs]}

let prepend_string buffer s =
  let cs = chunk_of_string s in
  {buffer with chunks = cs::buffer.chunks}

let insert_string buffer s loc =
  let cs = chunk_of_string s in
  let rec loop rem acc = function
    | [] when rem <= 0
         -> cs::(List.rev acc)
    | [] -> failwith "Location out of bounds"
    | x::xs when rem >= chunk_length x 
         -> loop (rem-chunk_length x) (x::acc) xs
    | xs when rem <= 0
         -> (List.rev acc)@cs::xs
    | x::xs ->
        let x1, x2 = split_chunk rem x in
        (List.rev acc)@x1::cs::x2::xs
  in loop loc [] buffer.chunks



