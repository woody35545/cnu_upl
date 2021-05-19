module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
        match e with
        | Bool b -> (*Bool of bool*)
                       begin
                               match b with
                               | true -> BoolV true  
                               | false -> BoolV false
                       end 
        | Num n -> NumV n
        | Add (expr1,expr2) ->
                        let expr1_res = interp_e s expr1 in 
                        let expr2_res = interp_e s expr2 in
                        begin
                                match expr1_res , expr2_res with
                                | NumV r1 , NumV r2 -> NumV (expr1_res + expr2_res) 
                                | _ -> failwith (Format.asprintf "Invalid addition: %a + %a" (Ast.pp_e expr1) (Ast.pp_e expr2)) 
                        end     
        | Sub (expr1,expr2) -> 
                        let expr1_res = interp_e s expr1 in
                        let expr2_res = interp_e s expr2 in
                        begin
                                match expr1_res, expr2_res with
                                | NumV r1 , NumV r2 -> NumV (expr1_res - expr2_res)
                                | _ -> failwith (Format.asprintf "Invalid subtraction %a - %a" (Ast.pp_e expr1) (Ast.pp_e expr2)) 
                        end   
        | Id i -> Store.find i s
        | LetIn (x, expr1, expr2) ->
                        let newstore = Store.insert x (interp_e s expr1) s in 
                        interp_e newstore expr2
        | Fun (param,body) -> ClousreV (param,body,s) 
        | App (expr1,expr2) -> 
                        begin
                                match expr1 with
                                | Fun ->
                                                let (param, expr, store) = interp_e s expr1 in 
                                                let newstore = Store.insert param expr2 s in
                                                interp_e newstore expr 
                                | _ -> failwith (Format.asprintf "Not a function %a" Ast.pp_e expr1) 
                        end 
        | Lt (expr1,expr2) ->
                        let expr1_res = interp_e s expr1 in
                        let expr2_res = interp_e s expr2 in 
        begin
                match expr1_res, expr2_res with
                | NumV r1 , NumV r2 -> 
                if r1 < r2 then true else false     
               | _ -> failwith ""

        end                
                        
       
let interp (p : Ast.fvae) : Store.value = 
        match p with
        | Prog p -> interp_e [] p 
        
