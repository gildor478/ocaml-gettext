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

module AstGettextMatch =
  struct
    type t = unit
    
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
          e, ExStr(_, singular)
          ) when is_like e s_functions ->
          (* Add a singular / default domain string *)
          print_string ("Singular: "^singular);
          print_newline ();
          print_newline ()
      | ExApp(_, 
          ExApp(_, e, _), ExStr(_, singular)
          ) when is_like e gettext_functions ->
          (* Add a singular / default domain string *)
          print_string ("Singular: "^singular);
          print_newline ();
          print_newline ()
      | ExApp(_, 
          ExApp(_, e, ExStr(_, singular)), ExStr(_, plural)
          ) when is_like e sn_functions ->
          (* Add a plural / default domain string *)
          print_string ("Singular: "^singular);
          print_newline ();
          print_string ("Plural: "^plural);
          print_newline ();
          print_newline ()
      | ExApp(_, 
          ExApp(_, ExApp(_, e, _), ExStr(_, singular)), ExStr(_, plural)
          ) when is_like e ngettext_functions ->
          (* Add a plural / default domain string *)
          print_string ("Singular: "^singular);
          print_newline ();
          print_string ("Plural: "^plural);
          print_newline ();
          print_newline ()
      | ExApp(_, 
          ExApp(_, ExApp(_, e, _), ExStr(_, domain)), ExStr(_, singular)
          ) when is_like e dgettext_functions ->
          (* Add a singular / defined domain string *)
          print_string ("Domain: "^domain);
          print_newline ();
          print_string ("Singular: "^singular);
          print_newline ();
          print_newline ()
      | ExApp(_, 
          ExApp(_, ExApp(_, ExApp(_, e, _), ExStr(_, domain)), ExStr(_, singular)), ExStr(_, plural)
          ) when is_like e dngettext_functions ->
          (* Add a plural / defined domain string *)
          print_string ("Domain: "^domain);
          print_newline ();
          print_string ("Singular: "^singular);
          print_newline ();
          print_string ("Plural: "^plural);
          print_newline ();
          print_newline ()
      | _ ->
          ()

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

let gettext_interf lst =
  AstGettext.interf () lst
;;

let gettext_implem lst = 
  AstGettext.implem () lst
;;

(* Register Pcaml printer *)

Pcaml.print_interf := gettext_interf
;;

Pcaml.print_implem := gettext_implem
;;
