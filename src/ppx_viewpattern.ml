open Ppxlib
open Ast_builder.Default

let cnt = ref 0

let pat_fold_mapper = object (self)
  inherit [(expression * expression * pattern) list] Ast_traverse.fold_map as super

  method! pattern pat acc =
    let loc = pat.ppat_loc in
    match pat with
    | [%pat? [%view? [%p? inner] when [%e? view]]] ->
      let viewpattern_label = "__view_" ^ string_of_int !cnt in
      incr cnt;
      let (inner', accinner) = self#pattern inner [] in
      (pvar ~loc viewpattern_label, accinner @ (evar ~loc viewpattern_label, view, inner') :: acc)
    | _ -> super#pattern pat acc
end

let impl_mapper: Ast_traverse.map = object (self)
  inherit Ast_traverse.map as super

  method private case_with_fallback case fallback_cases =
    let (pat', acc) = pat_fold_mapper#pattern case.pc_lhs [] in
    let fallback_label = "__view_fallback" in
    let pat' =
      if acc = [] then
        pat' (* avoid unused alias *)
      else (
        let loc = pat'.ppat_loc in
        ppat_alias ~loc pat' (Located.mk ~loc fallback_label)
      )
    in
    let fallback_case ~loc =
      {pc_lhs = ppat_any ~loc; pc_guard = None; pc_rhs = pexp_match ~loc (evar ~loc fallback_label) fallback_cases}
    in
    let (guard', rhs') = List.fold_left (fun (guard, rhs') (name, view, inner) ->
        let loc = inner.ppat_loc in
        (None, pexp_match ~loc (eapply ~loc (self#expression view) [name]) [
          {pc_lhs = inner; pc_guard = guard; pc_rhs = rhs'};
          fallback_case ~loc
        ])
      ) (Option.map self#expression case.pc_guard, self#expression case.pc_rhs) acc
    in
    {pc_lhs = pat'; pc_guard = guard'; pc_rhs = rhs'}

  method! cases cases =
    List.fold_right (fun case fallback_cases ->
        self#case_with_fallback case fallback_cases :: fallback_cases
      ) cases []

  method! expression expr =
    match expr.pexp_desc with
    | Pexp_fun (label, default, pat, expr) ->
      let (pat', acc) = pat_fold_mapper#pattern pat [] in
      let rhs' = List.fold_left (fun rhs' (name, view, inner) ->
          let loc = inner.ppat_loc in
          pexp_match ~loc (eapply ~loc (self#expression view) [name]) [
            {pc_lhs = inner; pc_guard = None; pc_rhs = rhs'}
          ]
        ) (self#expression expr) acc
      in
      {expr with pexp_desc = Pexp_fun (label, default, pat', rhs')}
    | Pexp_let (flag, bindings, expr) ->
      let (acc, bindings') = List.fold_right (fun binding (acc, bindings') ->
         let (pat', acc) = pat_fold_mapper#pattern binding.pvb_pat acc in
         (acc, {binding with pvb_pat = pat'; pvb_expr = self#expression binding.pvb_expr} :: bindings')
        ) bindings ([], [])
      in
      let rhs' = List.fold_left (fun rhs' (name, view, inner) ->
          let loc = inner.ppat_loc in
          pexp_match ~loc (eapply ~loc (self#expression view) [name]) [
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
