(*#include <stdio.h>
#include <stdlib.h>
#include "xsetenv.h"
#define _(string) gettext (string)

int main (argc, argv)
  int argc;
  char *argv[];
{
  int n = atoi (argv[2]);

  xsetenv ("LC_ALL", argv[1], 1);
  if (setlocale (LC_ALL, "") == NULL)
    {
      fprintf (stderr, "Couldn't set locale.\n");
      exit (77);
    }

  textdomain ("prog");
  bindtextdomain ("prog", ".");

  printf (_("'Your command, please?', asked the waiter."));
  printf ("\n");

  printf (ngettext ("a piece of cake", "%d pieces of cake", n), n);
  printf ("\n");

  printf (_("%s is replaced by %s."), "FF", "EUR");
  printf ("\n");

  exit (0);
}*)

open Unix;;
open Camlgettext;;

let _ = putenv "LC_ALL" (Array.get Sys.argv 1)
in
let _ = setlocale LC_ALL ""
in
let _ = textdomain "prog"
in
let _ = bindtextdomain "prog" "."
in
print_string (gettext "'Your command, please?', asked the waiter.");
print_newline ()
