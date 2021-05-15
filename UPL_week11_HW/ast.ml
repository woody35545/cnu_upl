module F = Format

type expr = 
  | Num of int
  | Add of expr * expr  
  | Sub of expr * expr
  | Id of string
  | LetIn of string * expr * expr
  | App of expr * expr
  | Fun of string * expr
  | Lt of expr * expr

type fvae =
  | Prog of expr

let rec pp_e fmt e = 
  match e with
  | Num n -> F.fprintf fmt "(Num %d)" n 
  | Add (e1, e2) -> F.fprintf fmt "(Add %a %a)" pp_e e1 pp_e e2
  | Sub (e1, e2) -> F.fprintf fmt "(Sub %a %a)" pp_e e1 pp_e e2
  | Id x -> F.fprintf fmt "(Id %s)" x
  | LetIn (x, e1, e2) -> F.fprintf fmt "(LetIn %s %a %a)" x pp_e e1 pp_e e2
  | App (e1, e2) -> F.fprintf fmt "(App %a %a)" pp_e e1 pp_e e2
  | Fun (p, e) -> F.fprintf fmt "(Fun %s -> %a)" p pp_e e
  | Lt (e1, e2) -> F.fprintf fmt "(Lt %a < %a)" pp_e e1 pp_e e2 

let pp fmt (Prog e) = 
  F.fprintf fmt "(Prog %a)" pp_e e

