(** Signature of module for charset conversion *)

open GettextTypes;;

module type CHARSET_TYPE = 
  sig
    type encoding = string
    type u
    
    (** create in_enc out_enc : create a new charset converter from charset 
        in_enc to out_enc.
    *)
    val create : t -> encoding -> encoding -> u

    (** recode str enc : return a transcoded string according to enc.
    *)
    val recode : u -> string  -> string
  end
;;

module Dummy : CHARSET_TYPE =
  struct
    type encoding = string
    type u = ()

    let create t in_enc out_enc = ()

    let recode () str = str
  end
;;
