open Ppxlib
open Ast_builder.Default


type viewpattern = {
  var: expression;
  view: expression;
  pat: pattern;
}

class viewpattern_extractor = object (self)
  inherit [viewpattern list] Ast_traverse.fold_map as super

  val mutable fresh_label_count = 0

  method private fresh_label =
    let label = "__view_" ^ string_of_int fresh_label_count in
    fresh_label_count <- fresh_label_count + 1;
    label

  method! pattern pat acc =
    match pat with
    | [%pat? [%view? [%p? viewpattern_pat] when [%e? view]]] ->
      let loc = pat.ppat_loc in
      let viewpattern_label = self#fresh_label in
      let (viewpattern_pat', pat_acc) = self#pattern viewpattern_pat [] in
      let viewpattern = {var = evar ~loc viewpattern_label; view; pat = viewpattern_pat'} in
      let pat' = pvar ~loc viewpattern_label in
      (pat', pat_acc @ viewpattern :: acc)
    | _ -> super#pattern pat acc
end

class viewpattern_impl = object (self)
  inherit Ast_traverse.map as super

  val viewpattern_extractor = new viewpattern_extractor

  method private case_with_fallback case fallback_cases =
    let (pat', acc) = viewpattern_extractor#pattern case.pc_lhs [] in
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
    let (guard', rhs') = List.fold_left (fun (guard, rhs') {var = name; view; pat = inner} ->
        let loc = inner.ppat_loc in
        (None, pexp_match ~loc (eapply ~loc (self#expression view) [name]) [
          {pc_lhs = inner; pc_guard = guard; pc_rhs = rhs'};
          fallback_case ~loc
        ])
      ) (self#option self#expression case.pc_guard, self#expression case.pc_rhs) acc
    in
    {pc_lhs = pat'; pc_guard = guard'; pc_rhs = rhs'}

  method! cases cases =
    List.fold_right (fun case fallback_cases ->
        self#case_with_fallback case fallback_cases :: fallback_cases
      ) cases []

  method! expression_desc = function
    | Pexp_fun (label, default, pat, expr) ->
      let (pat', acc) = viewpattern_extractor#pattern pat [] in
      let rhs' = List.fold_left (fun rhs' {var = name; view; pat = inner} ->
          let loc = inner.ppat_loc in
          [%expr let [%p inner] = [%e self#expression view] [%e name] in [%e rhs']]
        ) (self#expression expr) acc
      in
      Pexp_fun (label, default, pat', rhs')
    | Pexp_let (flag, bindings, expr) ->
      let (acc, bindings') = List.fold_right (fun binding (acc, bindings') ->
         let (pat', acc) = viewpattern_extractor#pattern binding.pvb_pat acc in
         (acc, {binding with pvb_pat = pat'; pvb_expr = self#expression binding.pvb_expr} :: bindings')
        ) bindings ([], [])
      in
      let rhs' = List.fold_left (fun rhs' {var = name; view; pat = inner} ->
          let loc = inner.ppat_loc in
          [%expr let [%p inner] = [%e self#expression view] [%e name] in [%e rhs']]
        ) (self#expression expr) acc
      in
      Pexp_let (flag, bindings', rhs')
    | expr_desc -> super#expression_desc expr_desc
end

let impl = (new viewpattern_impl)#structure

let () =
  Driver.register_transformation ~impl "viewpattern"
