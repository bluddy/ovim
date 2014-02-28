open Batteries

module IntMap = Map.Make(struct type t=int let compare a b = a-b end)

type filepath = string

type buffer = {
  file_name : filepath option;
  contents : char array; (* for now *)
  line_index : int IntMap.t;
}

type window = {
  buffer : buffer;
  width : int;
  height : int;
  location : int;
  line_offset : int;
}

type window_col = window list

let contents_of_string s =
  let len = String.length s in
  let a = Array.create len '\n' in
  for i=0 to len-1 do
    a.(i) <- s.[i]
  done;
  a

let line_index_of_contents contents =
  let acc, _, _ =
    Array.fold_left (fun (acc,line,index) c ->
      if c='\n' then 
        let acc' = IntMap.add line (index+1) acc in
        acc', line+1, index+1
      else
        acc,  line,   index+1)
      (IntMap.empty, 1, 0)
      contents
  in acc
    
let buffer_of_file fname s = 
  let contents = contents_of_string s in
  let line_index = line_index_of_contents contents in
  {file_name=Some fname; contents; line_index}

let load_buffer fname = 
  let s = File.with_file_in fname IO.read_all in
  buffer_of_file fname s
