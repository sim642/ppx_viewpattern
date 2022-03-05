open Ppxlib
open Ast_builder.Default

let cnt = ref 0

let pat_fold_mapper = object
  inherit [(string * expression * pattern) list] Ast_traverse.fold_map as super

  method! pattern pat acc =
    let loc = pat.ppat_loc in
    match pat with
    | [%pat? [%view? [%p? inner] when [%e? view]]] ->
      let name = "foo" ^ string_of_int !cnt in
      incr cnt;
      (pvar ~loc name, (name, view, inner) :: acc)
    | _ -> super#pattern pat acc
end

let impl_mapper = object (self)
  inherit Ast_traverse.map as super

  (* method! case (case: case) = *)
  method do_case case rest =
    let (pat', acc) = pat_fold_mapper#pattern case.pc_lhs [] in
    let pat' =
      if acc = [] then
        pat'
      else
        let loc = pat'.ppat_loc in
        ppat_alias ~loc pat' (Located.mk ~loc "outer")
    in
    let rhs' = List.fold_left (fun rhs' (name, view, inner) ->
        let loc = inner.ppat_loc in
        pexp_match ~loc (eapply ~loc view [evar ~loc name]) [
          {pc_lhs = inner; pc_guard = None; pc_rhs = rhs'};
          {pc_lhs = ppat_any ~loc; pc_guard = None; pc_rhs = pexp_match ~loc (evar ~loc "outer") rest}
        ]
      ) (self#expression case.pc_rhs) acc
    in
    {case with pc_lhs = pat'; pc_rhs = rhs'}

  method! cases cases =
    List.fold_right (fun case rest ->
        self#do_case case rest :: rest
      ) cases []

  method! expression expr =
    match expr.pexp_desc with
    | Pexp_fun (label, default, pat, expr) ->
      let (pat', acc) = pat_fold_mapper#pattern pat [] in
      let rhs' = List.fold_left (fun rhs' (name, view, inner) ->
          let loc = inner.ppat_loc in
          pexp_match ~loc (eapply ~loc view [evar ~loc name]) [
            {pc_lhs = inner; pc_guard = None; pc_rhs = rhs'}
          ]
        ) (self#expression expr) acc
      in
      {expr with pexp_desc = Pexp_fun (label, default, pat', rhs')}
    | Pexp_let (flag, bindings, expr) ->
      let (acc, bindings') = List.fold_left_map (fun acc binding ->
         let (pat', acc) = pat_fold_mapper#pattern binding.pvb_pat acc in
         (acc, {binding with pvb_pat = pat'; pvb_expr = self#expression binding.pvb_expr})
        ) [] bindings
      in
      let rhs' = List.fold_left (fun rhs' (name, view, inner) ->
          let loc = inner.ppat_loc in
          pexp_match ~loc (eapply ~loc view [evar ~loc name]) [
            {pc_lhs = inner; pc_guard = None; pc_rhs = rhs'}
          ]
        ) (self#expression expr) acc
      in
      {expr with pexp_desc = Pexp_let (flag, bindings', rhs')}
    | _ -> super#expression expr
end

let impl (str: structure): structure =
  impl_mapper#structure str

let () =
  Driver.register_transformation ~impl "viewpattern"
