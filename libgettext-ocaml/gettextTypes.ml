type textdomain = string
;;

type locale = string
;;

type dir = string
;;

type filename = string
;;

type codeset = string
;;

type category =
    LC_CTYPE
  | LC_NUMERIC
  | LC_TIME
  | LC_COLLATE
  | LC_MONETARY
  | LC_MESSAGES
  | LC_ALL
;;
  
type failsafe = 
    Ignore 
  | InformStderr 
  | RaiseException
;;

type endianess = 
    BigEndian 
  | LittleEndian
;;

(* Specification of .MO file *)
(*
   From GNU Gettext documentation 
   ( http://www.gnu.org/software/gettext/manual/html_mono/gettext.html#SEC136 ).

   Format of MO file :

        byte
             +------------------------------------------+
          0  | magic number = 0x950412de                |
             |                                          |
          4  | file format revision = 0                 |
             |                                          |
          8  | number of strings                        |  == N
             |                                          |
         12  | offset of table with original strings    |  == O
             |                                          |
         16  | offset of table with translation strings |  == T
             |                                          |
         20  | size of hashing table                    |  == S
             |                                          |
         24  | offset of hashing table                  |  == H
             |                                          |
             .                                          .
             .    (possibly more entries later)         .
             .                                          .
             |                                          |
          O  | length & offset 0th string  ----------------.
      O + 8  | length & offset 1st string  ------------------.
              ...                                    ...   | |
O + ((N-1)*8)| length & offset (N-1)th string           |  | |
             |                                          |  | |
          T  | length & offset 0th translation  ---------------.
      T + 8  | length & offset 1st translation  -----------------.
              ...                                    ...   | | | |
T + ((N-1)*8)| length & offset (N-1)th translation      |  | | | |
             |                                          |  | | | |
          H  | start hash table                         |  | | | |
              ...                                    ...   | | | |
  H + S * 4  | end hash table                           |  | | | |
             |                                          |  | | | |
             | NUL terminated 0th string  <----------------' | | |
             |                                          |    | | |
             | NUL terminated 1st string  <------------------' | |
             |                                          |      | |
              ...                                    ...       | |
             |                                          |      | |
             | NUL terminated 0th translation  <---------------' |
             |                                          |        |
             | NUL terminated 1st translation  <-----------------'
             |                                          |
              ...                                    ...
             |                                          |
             +------------------------------------------+

*)

type mo_header = {
  endianess                : endianess;
  file_format_revision     : int32;
  number_of_strings        : int32;
  offset_table_strings     : int32;
  offset_table_translation : int32;
  size_of_hashing_table    : int32;
  offset_of_hashing_table  : int32;
}
;;

(* Details associated with "" *)
(* Project-Id-Version: PACKAGE VERSION\n        *)
(* Report-Msgid-Bugs-To: \n                     *)
(* POT-Creation-Date: 2004-05-31 16:53+0200\n   *)
(* PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n    *)
(* Last-Translator: FULL NAME <EMAIL@ADDRESS>\n *)
(* Language-Team: LANGUAGE <LL@li.org>\n        *)
(* MIME-Version: 1.0\n                          *)
(* Content-Type: text/plain; charset=CHARSET\n  *)
(* Content-Transfer-Encoding: 8bit\n            *)
(* Plural-Forms: specific ( 0 is false and 1 is *)
(* true                                         *)

type mo_translation = {
  project_id_version        : string option;
  report_msgid_bugs_to      : string option;
  pot_creation_date         : string option;
  po_revision_date          : string option;
  last_translator           : string option;
  language_tream            : string option;
  mime_version              : string option;
  content_type              : string option;
  content_transfer_encoding : string option;
  plural_forms              : string option;
  (* The only interesting fields *)
  (* Those field are precomputed for regular use *)
  content_type_charset      : string;
  nplurals                  : int;
  fun_plural_forms          : int -> int; 
}
;;

type translation = 
  Singular of string * string
| Plural of string * string * string list
;;

type po_content =
  Domain of string * (translation list)
| NoDomain of translation list
;;


module MapString = Map.Make (struct
  type t      = string
  let compare = compare
end)
;;
 
module MapTextdomain = Map.Make (struct
  type t      = string
  let compare = compare
end)
;;
  
module MapCategory = Map.Make (struct 
  type t      = category
  let compare a b = 
    let val_category x = 
      match x with 
        LC_CTYPE    -> 0 
      | LC_NUMERIC  -> 1
      | LC_TIME     -> 2
      | LC_COLLATE  -> 3
      | LC_MONETARY -> 4
      | LC_MESSAGES -> 5
      | LC_ALL      -> 6
    in
    compare (val_category a) (val_category b)
end)
;;

(** Core types of ocaml-gettext library *)

type t = {
  failsafe    : failsafe;
  textdomains : ((codeset option) * (dir option)) MapTextdomain.t;
  categories  : locale MapCategory.t;
  language    : locale option;
  codeset     : codeset;
  dirs        : dir list;
  default     : textdomain;
}
;;
  
type t' = textdomain option -> string -> (string * int) option -> category -> string
;;

type dependencies = (textdomain * (codeset option) * (dir option)) list
;;

module type Init = 
  sig
    val textdomain : textdomain
    val codeset    : codeset option
    val dir        : dir option
    val dependencies : dependencies
  end
;;
  
type realize = t -> t'
;;

module type InitProgram =
  sig
    include Init

    val realize : realize
  end
;;

