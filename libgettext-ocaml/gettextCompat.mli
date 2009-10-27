val format_of_string : 'a -> 'b
val bindtextdomain :
  GettextTypes.MapTextdomain.key ->
  GettextTypes.dir -> GettextTypes.t -> GettextTypes.t
val bind_textdomain_codeset :
  GettextTypes.MapTextdomain.key ->
  GettextTypes.codeset -> GettextTypes.t -> GettextTypes.t
val textdomain : GettextTypes.textdomain -> GettextTypes.t -> GettextTypes.t
val get_textdomain : GettextTypes.t -> GettextTypes.textdomain
val gettext :
  (bool -> 'a option -> 'b -> 'c option -> GettextCategory.category -> 'd) ->
  'b -> 'd
val fgettext :
  (bool -> 'a option -> 'b -> 'c option -> GettextCategory.category -> 'd) ->
  'b -> 'e
val dgettext :
  (bool -> 'a option -> 'b -> 'c option -> GettextCategory.category -> 'd) ->
  'a -> 'b -> 'd
val fdgettext :
  (bool -> 'a option -> 'b -> 'c option -> GettextCategory.category -> 'd) ->
  'a -> 'b -> 'e
val dcgettext :
  (bool -> 'a option -> 'b -> 'c option -> 'd -> 'e) -> 'a -> 'b -> 'd -> 'e
val fdcgettext :
  (bool -> 'a option -> 'b -> 'c option -> 'd -> 'e) -> 'a -> 'b -> 'd -> 'f
val ngettext :
  (bool ->
   'a option -> 'b -> ('c * 'd) option -> GettextCategory.category -> 'e) ->
  'b -> 'c -> 'd -> 'e
val fngettext :
  (bool ->
   'a option -> 'b -> ('c * 'd) option -> GettextCategory.category -> 'e) ->
  'b -> 'c -> 'd -> 'f
val dngettext :
  (bool ->
   'a option -> 'b -> ('c * 'd) option -> GettextCategory.category -> 'e) ->
  'a -> 'b -> 'c -> 'd -> 'e
val fdngettext :
  (bool ->
   'a option -> 'b -> ('c * 'd) option -> GettextCategory.category -> 'e) ->
  'a -> 'b -> 'c -> 'd -> 'f
val dcngettext :
  (bool -> 'a option -> 'b -> ('c * 'd) option -> 'e -> 'f) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f
val fdcngettext :
  (bool -> 'a option -> 'b -> ('c * 'd) option -> 'e -> 'f) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'g
