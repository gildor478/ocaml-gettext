open GettextTypes;;

let split_plural str =
  let rec split_plural_one start =
    let next_sep = 
      try
        String.index_from str start '\000' 
      with Not_found ->
        String.length str
    in
    let new_plural = String.sub str start (next_sep - start)
    in
    if (next_sep + 1) >= String.length str then
      [new_plural]
    else
      new_plural :: ( split_plural_one (next_sep + 1) )
  in
  split_plural_one 0
;;

let fail_or_continue failsafe exc cont_value =
  match failsafe with
    Ignore ->
      cont_value
  | InformStderr exc_printer ->
      (
        prerr_string (exc_printer exc);
        prerr_newline ();
        cont_value
      )
  | RaiseException ->
      raise exc
;;

