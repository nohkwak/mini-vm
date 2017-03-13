(*
   Mini Language Compiler (Assembler)

   To run: ocaml str.cma compiler.ml <asm file> <output file>
*)

type line = { no : int; content : string}

let max_oprs = 3
let space = Str.regexp "\\( \\|\t\\)+"
let delim = Str.regexp "\\( \\|\t\\)*,\\( \\|\t\\)*"
let reg = Str.regexp "r\\([0-9]+\\)"
let imm = Str.regexp "[0-9]+"

let usage () =
  print_endline "Usage : ocaml str.cma compiler.ml <asm file> <output file>"

let error line msg =
  Printf.printf "[Errror] Line %d : %s\n\t%s" line.no msg line.content;
  exit 1

let opcode_to_int = function
  | "halt" -> 0x00
  | "load" -> 0x10
  | "store" -> 0x20
  | "move" -> 0x30
  | "puti" -> 0x40
  | "add" -> 0x50
  | "sub" -> 0x60
  | "gt" -> 0x70
  | "ge" -> 0x80
  | "eq" -> 0x90
  | "ite" -> 0xa0
  | "jump" -> 0xb0
  | "puts" -> 0xc0
  | "gets" -> 0xd0
  | opcode -> failwith ("Invalid opcode: " ^ opcode)

let out_opcode oc opcode =
  opcode_to_int opcode |> output_byte oc

let rec out_no_opr oc cnt =
  if cnt <= 0 then ()
  else (output_byte oc 0; out_no_opr oc (cnt-1))

let out_reg oc num = int_of_string num |> output_byte oc

let out_imm = out_reg

let out_operand oc operand =
  let o = String.lowercase operand in (* only consider lower cases *)
  if Str.string_match reg o 0 then Str.matched_group 1 o |> out_reg oc
  else if Str.string_match imm o 0 then out_imm oc o
  else failwith ("Invalid operand: " ^ operand)

let out oc opcode operands =
  assert (List.length operands <= 3);
  out_opcode oc opcode;
  List.iter (out_operand oc) operands;
  out_no_opr oc (max_oprs - List.length operands)

(* Check if the types (reg/imm) of operands are valid *)
let check_type operand operands line =
  (* TODO : fill in *)
  ()

let parse inpath outpath =
  let ic = open_in inpath in
  let oc = open_out outpath in
  try
    let line_no = ref 0 in
    while true do
      let l = input_line ic in
      let _ = line_no := !line_no + 1 in
      let line = {no = !line_no; content = l} in
      match Str.split space l with
      | opcode :: operands_str ->
          let operands = Str.split delim (String.concat "" operands_str) in
          check_type opcode operands line;
          out oc opcode operands
      | _ -> failwith "Str.Split failure"
    done
  with End_of_file -> (close_in ic; close_out oc)

let main =
  if Array.length Sys.argv <> 3 then (usage(); exit 1)
  else parse Sys.argv.(1) Sys.argv.(2)
