module F = Format

(* Test cases *)
let _ = 
  try
  let open Ast in
  let open Interpreter in
  let open ParserMain in
  let p1 = parse "true" in
  let p2 = parse "false" in
  let p3 = parse "1 < 3" in
  let p4 = parse "3 < 2" in
  let p5 = parse "1 < (0 + 1)" in
  let p6 = parse "(1 - 1) < (0 + 1)" in
  let p7 = parse "if true then 1 else 3" in
  let p8 = parse "if false then 1 else 3" in
  let p9 = parse "if (1 - 1) < (0 + 1) then 0 + 7 else 3" in
  let p10 = parse "if (1 + 0) then 0 + 7 else 3" in
  let p11 = parse "if true then 1 else 3 + 10"in
  let p12 = parse "if false then 1 else 3 + 10" in
  let p13 = parse "let x = 3 in if x < 4 then x + 10 else x - 10" in
  let p14 = parse "let x = 3 in if let x = 2 in x < 3 then x + 10 else x - 10" in
  let p15 = parse "let x = 3 in if let x = 2 in x < 3 then let y = 10 in x + y else let y = 99 in x - y" in
  let p16 = parse "let x = 3 in if let y = 2 in y < x then x + 10 else x - 10" in
  let p17 = parse "let x = (fun x -> x + 10) in x (if let x = 2 in x < 3 then x 1 else x 99)" in
  let p18 = parse "let x = (if let x = 2 in x < 3 then (fun x -> x + 10) else (fun x -> x - 10)) in x 9" in
  let _ = F.printf "AST: %a\n" pp p1 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p1) in (* <λx.(Fun y -> (Id x)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p2 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p2) in (* <λx.(Fun y -> (Id y)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p3 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p3) in (* <λx.(Fun y -> (Id x)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p4 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p4) in (* <λx.(Fun y -> (Id y)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p5 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p5) in (* <λx.(Fun y -> (Id y)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p6 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p6) in (* <λx.(Fun y -> (Id x)), [ ]> *)
  let _ = F.printf "AST: %a\n" pp p7 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p7) in (* 1 *)
  let _ = F.printf "AST: %a\n" pp p8 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p8) in (* 3 *)
  let _ = F.printf "AST: %a\n" pp p9 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p9) in (* 7 *)
  let _ = F.printf "AST: %a\n" pp p10 in
  let _ = try F.printf "RES: %a\n" Store.pp_v (interp p10) with Failure e -> F.printf "Runtime Error : %s\n" e in (* Not a function: (Add (Num 1) (Num 0)) *)
  let _ = F.printf "AST: %a\n" pp p11 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p11) in (* 1 *)
  let _ = F.printf "AST: %a\n" pp p12 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p12) in (* 13 *)
  let _ = F.printf "AST: %a\n" pp p13 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p13) in (* 13 *)
  let _ = F.printf "AST: %a\n" pp p14 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p14) in (* 13 *)
  let _ = F.printf "AST: %a\n" pp p15 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p15) in (* 13 *)
  let _ = F.printf "AST: %a\n" pp p16 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p16) in (* 13 *)
  let _ = F.printf "AST: %a\n" pp p17 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p17) in (* 21 *)
  let _ = F.printf "AST: %a\n" pp p18 in
  let _ = F.printf "RES: %a\n" Store.pp_v (interp p18) in (* 19 *)
  ()
  with 
  | Lexer.LexError msg -> F.printf "[ERR] Undefined token: %s\n" msg
  | Parser.Error -> F.printf "[ERR] Wrong grammar\n" 
  
