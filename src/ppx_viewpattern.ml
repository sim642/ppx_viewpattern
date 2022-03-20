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

let attr_warning ~loc str =
  let structure = {pstr_desc = Pstr_eval (estring ~loc str, []); pstr_loc = loc} in
  { attr_name = { txt = "ocaml.warning"; loc; };
    attr_payload = PStr [structure];
    attr_loc = loc;
  }

class viewpattern_impl = object (self)
  inherit Ast_traverse.map as super

  val viewpattern_extractor = new viewpattern_extractor

  method private case_with_fallback case fallback_cases =
    let (lhs', viewpatterns) = viewpattern_extractor#pattern case.pc_lhs [] in
    let fallback_label = "__view_fallback" in
    let lhs' =
      if viewpatterns = [] then
        lhs' (* avoid unused alias *)
      else (
        let loc = lhs'.ppat_loc in
        ppat_alias ~loc lhs' (Located.mk ~loc fallback_label)
      )
    in
    let fallback_case ~loc =
      {pc_lhs = ppat_any ~loc; pc_guard = None; pc_rhs = pexp_match ~loc (evar ~loc fallback_label) fallback_cases} (* fine if fallback_cases contains exception patterns, won't be used for __view_fallback variable anyway *)
    in
    let (guard', rhs') = List.fold_left (fun (guard, rhs) {var; view; pat} ->
        let loc = pat.ppat_loc in
        let rhs' =
          pexp_match ~loc (eapply ~loc (self#expression view) [var]) [
            {pc_lhs = pat; pc_guard = guard; pc_rhs = rhs};
            fallback_case ~loc
          ]
        in
        (None, rhs')
      ) (self#option self#expression case.pc_guard, self#expression case.pc_rhs) viewpatterns
    in
    ({pc_lhs = lhs'; pc_guard = guard'; pc_rhs = rhs'}, viewpatterns <> [])

  method private cases_contains_view ~inner_fallback_cases cases =
    List.fold_right (fun case (outer_fallback_cases, inner_fallback_cases, contains_view) ->
        let (case', case_contains_view) = self#case_with_fallback case inner_fallback_cases in
        (case' :: outer_fallback_cases, case' :: inner_fallback_cases, contains_view || case_contains_view)
      ) cases ([], inner_fallback_cases, false)
    |> fun (cases', _, contains_view) -> (cases', contains_view)

  method private cases_attributes ?(inner_fallback_cases=[]) cases loc =
    match self#cases_contains_view ~inner_fallback_cases cases with
    | (cases', true) -> (cases', [attr_warning ~loc "-redundant-case"; attr_warning ~loc "-partial-match"])
    | (cases', false) -> (cases', [])

  method private expression_viewpatterns expr viewpatterns =
    List.fold_left (fun expr {var; view; pat} ->
        let loc = pat.ppat_loc in
        [%expr let [%p pat] = [%e self#expression view] [%e var] in [%e expr]]
      ) (self#expression expr) viewpatterns

  method! expression ({ pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } as expr) =
    let pexp_loc = self#location pexp_loc in
    let pexp_loc_stack = self#location_stack pexp_loc_stack in
    let pexp_attributes = self#attributes pexp_attributes in
    match pexp_desc with
    | Pexp_match (expr, cases) ->
      let expr' = self#expression expr in
      let (cases', attributes) = self#cases_attributes cases pexp_loc in
      {pexp_desc = Pexp_match (expr', cases'); pexp_loc; pexp_loc_stack; pexp_attributes = attributes @ pexp_attributes}

    | Pexp_function cases ->
      let (cases', attributes) = self#cases_attributes cases pexp_loc in
      {pexp_desc = Pexp_function cases'; pexp_loc; pexp_loc_stack; pexp_attributes = attributes @ pexp_attributes}

    | Pexp_try (expr, cases) ->
      let expr' = self#expression expr in
      let inner_fallback_cases =
        let loc = pexp_loc in
        [{pc_lhs = [%pat? e]; pc_guard = None; pc_rhs = [%expr raise e]}]
      in
      let (cases', _) = self#cases_attributes ~inner_fallback_cases cases pexp_loc in
      {pexp_desc = Pexp_try (expr', cases'); pexp_loc; pexp_loc_stack; pexp_attributes} (* TODO: attributes also somewhere here? *)

    | Pexp_fun (arg_label, default, param, body) ->
      let (param', viewpatterns) = viewpattern_extractor#pattern param [] in
      let body' = self#expression_viewpatterns body viewpatterns in
      {pexp_desc = Pexp_fun (arg_label, default, param', body'); pexp_loc; pexp_loc_stack; pexp_attributes}

    | Pexp_let (rec_flag, bindings, expr) ->
      let (bindings', viewpatterns) = List.fold_right (fun binding (bindings, viewpatterns) ->
          let (pat', viewpatterns') = viewpattern_extractor#pattern binding.pvb_pat viewpatterns in
          let binding' = {binding with pvb_pat = pat'; pvb_expr = self#expression binding.pvb_expr} in
          (binding' :: bindings, viewpatterns')
        ) bindings ([], [])
      in
      let expr' = self#expression_viewpatterns expr viewpatterns in
      {pexp_desc = Pexp_let (rec_flag, bindings', expr'); pexp_loc; pexp_loc_stack; pexp_attributes}

    | _ -> super#expression expr
end

let impl = (new viewpattern_impl)#structure

let () =
  Driver.register_transformation ~impl "viewpattern"
