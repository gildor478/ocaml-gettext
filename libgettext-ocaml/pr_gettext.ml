(* Extract the string which should be used for a gettext translation. 
   Functions that are looked for :
Functions     Arg 1      Arg 2      Arg 3      Arg 4      Arg 5      Arg 6   ...
s_            singular
f_            singular
sn_           singular   plural     _
fn_           singular   plural     _
gettext       _          singular
fgettext      _          singular
dgettext      _          domain     singular
fdgettext     _          domain     singular
dcgettext     _          domain     singular   _
fdcgettext    _          domain     singular   _
ngettext      _          singular   plural     _        
fngettext     _          singular   plural     _          
dngettext     _          domain     singular   plural     _
fdngettext    _          domain     singular   plural     _
dcngettext    _          domain     singular   plural     _          _   
fdcngettext   _          domain     singular   plural     _          _
*)

open MLast;;
open Format;;

module MapString = Map.Make(struct
  type t = string 
  let  compare s1 s2 = 
    String.compare s1 s2
end)
;;

type translations = ( MLast.loc * string option * string option ) list MapString.t
;;

let add_translation t loc singular plural domain =
  let elem = (loc,plural,domain)
  in
  try
    let lst = MapString.find singular t
    in
    MapString.add singular ( elem :: lst ) t
  with Not_found ->
    MapString.add singular [elem] t
;;

module AstGettextMatch =
  struct
    type t = translations
    
    let s_functions = ref [ "s_"; "f_" ]

    let sn_functions = ref [ "sn_"; "fn_" ]

    let gettext_functions = ref [ "gettext"; "fgettext" ]

    let dgettext_functions = ref [ "dgettext"; "fdgettext"; "dcgettext"; "fdcgettext" ]

    let ngettext_functions = ref [ "ngettext"; "fngettext" ]

    let dngettext_functions = ref [ "dngettext"; "fdngettext"; "dcngettext"; "fdcngettext" ]

    (* Check if the given node belong to the given functions *)
    let is_like e functions = 
      let function_name e =
        let rec check_module e =
          match e with
            ExAcc(_, ExUid(_, _), e) -> check_module e
          | ExUid(_, _) -> true
          | _ -> false
        in
        match e with
          ExLid(_, s) -> s
        | ExAcc(_, e, ExLid(_, s)) when check_module e -> s
        | _ -> raise Not_found
      in
      try
        List.mem (function_name e) !functions
      with Not_found ->
        false
    
    let id t x = t
    let ctyp = id 
    let row_field = id 
    let class_infos = id 
    let patt = id 
    let expr t e = 
      match e with
        ExApp(_, 
          e, ExStr(loc, singular)
          ) when is_like e s_functions ->
          (* Add a singular / default domain string *)
          add_translation t loc singular None None
      | ExApp(_, 
          ExApp(_, e, _), ExStr(loc, singular)
          ) when is_like e gettext_functions ->
          (* Add a singular / default domain string *)
          add_translation t loc singular None None
      | ExApp(_, 
          ExApp(_, e, ExStr(loc, singular)), ExStr(_, plural)
          ) when is_like e sn_functions ->
          (* Add a plural / default domain string *)
          add_translation t loc singular (Some plural) None
      | ExApp(_, 
          ExApp(_, ExApp(_, e, _), ExStr(loc, singular)), ExStr(_, plural)
          ) when is_like e ngettext_functions ->
          (* Add a plural / default domain string *)
          add_translation t loc singular (Some plural) None
      | ExApp(_, 
          ExApp(_, ExApp(_, e, _), ExStr(_, domain)), ExStr(loc, singular)
          ) when is_like e dgettext_functions ->
          (* Add a singular / defined domain string *)
          add_translation t loc singular None (Some domain)
      | ExApp(_, 
          ExApp(_, ExApp(_, ExApp(_, e, _), ExStr(_, domain)), ExStr(loc, singular)), ExStr(_, plural)
          ) when is_like e dngettext_functions ->
          (* Add a plural / defined domain string *)
          add_translation t loc singular (Some plural) (Some domain)
      | _ ->
          t

    let module_type = id 
    let sig_item = id 
    let with_constr = id 
    let module_expr = id 
    let str_item = id 
    let type_decl = id 
    let class_type = id 
    let class_sig_item = id 
    let class_expr = id 
    let class_str_item = id 
    let interf = id 
    let implem = id 
  end
;;

module AstGettext = Pr_ast_analyze.AstAnalyze(AstGettextMatch)
;;

let output_translations m = 
  let (fd,close_on_exit) = 
    match !Pcaml.output_file with
      Some f -> (open_out f,true)
    | None -> (stdout,false)
  in
  (
    if false then
      MapString.iter ( fun singular lst ->
        List.iter ( fun (loc,plural,domain) ->
          (
            match domain with 
              Some d -> output_string fd ("Domain: "^d^"\n")
            | None -> ()
          );
          output_string fd ("Singular: "^singular^"\n");
          (
            match plural with
              Some p -> output_string fd ("Plural: "^p^"\n")
            | None -> ()
          );
          output_string fd "\n";
        ) lst
      ) m
    else
      output_value fd m
  );
  flush fd;
  if close_on_exit then
    close_out fd
  else
    ()
;;
    

let gettext_interf lst =
  output_translations (AstGettext.interf MapString.empty lst)
;;

let gettext_implem lst = 
  output_translations (AstGettext.implem MapString.empty lst)
;;

(* Register Pcaml printer *)

Pcaml.print_interf := gettext_interf
;;

Pcaml.print_implem := gettext_implem
;;
