
type range = Int32.t * Int32.t 
;;

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
  | InformStderr of (exn -> string)
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


module MapString = Map.Make (struct
  type t      = string
  let compare = String.compare
end)
;;
 
module MapTextdomain = Map.Make (struct
  type t      = textdomain
  let compare = String.compare
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

type location = filename * int
;;

(** Base type of MO content : translation of string. The first string members are
    the string identifier ( singular form ). 
*)
type translation = 
  Singular of string * string 
| Plural of string * string * string list
;;

(** Types for the PO processing. The main difference with the type translation
    comes from the necessity of keeping a maximum of comment. 
*)
type po_translation = 
  PoSingular of (string list) * ( string list )
| PoPlural of (string list) * ( string list ) * ( string list ) list
;;

(** Mapping of PO content using the string identifier as the key.
*)
type translations = (location list * po_translation) MapString.t
;;

(** Content of a PO file. Since comments should be saved, and that we only save
    comments before and in message translation, we need to keep trace of the
    last comments, which is not attached to any translation 
*)
type po_content = {
  no_domain    : translations;
  domain       : translations MapTextdomain.t;
}
;;

(** Core types of ocaml-gettext library *)

type t = {
  failsafe    : failsafe;
  textdomains : ((codeset option) * (dir option)) MapTextdomain.t;
  categories  : locale MapCategory.t;
  language    : locale option;
  codeset     : codeset;
  path        : dir list;
  default     : textdomain;
}
;;
  
type t' = bool -> textdomain option -> string -> (string * int) option -> category -> string
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


(** Exceptions *)

(** From GettextCompile *)

(** filename wich generates the error message str *)
exception ProblemReadingFile of filename * string;;
(** while extracting filename the command str returns exit code i *)
exception ExtractionFailed of filename * string * int;;
(** while extracting filename the command receive signal i *)
exception ExtractionInterrupted of filename * string * int;;

(** From GettextDomain *)
exception DomainFileDoesntExist of filename list;; 

(** From GettextFormat *)
exception FormatInconsistent of string * string;;

(** From Gettext *)
exception GettextUninitialized;;

(** From GettextMo *)
exception InvalidOptions of Lexing.lexbuf * string;;
exception InvalidPlurals of Lexing.lexbuf * string;;
exception InvalidContentType of Lexing.lexbuf * string;;
exception InvalidTranslationSingular of string * int;;
exception InvalidTranslationPlural of (string list) * int;;
exception Junk of string * string list;;
exception EmptyEntry;;
exception InvalidMoFile;;
exception InvalidMoHeaderNegativeStrings;; 
exception InvalidMoHeaderTableStringOutOfBound of range * range;;
exception InvalidMoHeaderTableTranslationOutOfBound of range * range;;
exception InvalidMoHeaderTableTranslationStringOverlap of range * range;;
exception InvalidMoStringOutOfBound of int * int;;
exception InvalidMoTranslationOutOfBound of int * int;;
exception CannotOpenMoFile of string;;

(** From GettextPo *)
exception PoFileInvalid of string * Lexing.lexbuf * in_channel ;;
exception PoFileInvalidIndex of string * int;;
exception PoFileDoesntExist of string;;
exception PoInconsistentMerge of string * string;;

(** From GettextTranslate *)
exception GettextTranslateStringNotFound of string ;;


