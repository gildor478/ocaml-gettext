module C = Configurator.V1

let gettext_code =
  {|
#include <libintl.h>

int main() {
  gettext("abcd");
  return 0;
}
|}

let () =
  let brew_prefix =
    let default_path = "/usr/local" in
    let path =
      try
        let brew_exec = FileUtil.which "brew" in
        let i = Unix.open_process_in (brew_exec ^ " --prefix") in
        let s =
          match String.trim (In_channel.input_all i) with
          | "" -> default_path
          | s -> s
        in
        In_channel.close i;
        s
      with Not_found -> default_path
    in
    Filename.concat (Filename.concat path "opt") "gettext"
  in
  C.main ~name:"gettext" (fun c ->
      let is_working (c_flags, link_flags) =
        C.c_test c gettext_code ~link_flags ~c_flags
      in
      let c_flags, link_flags =
        try
          List.find is_working
            [
              (* Default that should work on standard Linux distributions. *)
              ([], []);
              (* MacOS with Homebrew.
               * The library is "keg-only" to prevent conflict with system
               * installed BSD gettext library, so we have to pull it from
               * /usr/local.
               * https://formulae.brew.sh/formula/gettext
               *)
              ( [ "-I" ^ Filename.concat brew_prefix "include" ],
                [ "-L" ^ Filename.concat brew_prefix "lib"; "-lintl" ] );
              (* MacOS with MacPorts. *)
              ([ "-I/opt/local/include" ], [ "-L/opt/local/lib"; "-lintl" ]);
              (* OpenBSD and FreeBSD. *)
              ([ "-I/usr/local/include" ], [ "-L/usr/local/lib"; "-lintl" ]);
              (* Cygwin. *)
              ([ "-I/usr/include" ], [ "-L/usr/lib"; "-lintl" ]);
            ]
        with Not_found -> C.die "no ways to compile with gettext library"
      in
      C.Flags.write_sexp "c_flags.sexp" c_flags;
      C.Flags.write_sexp "c_library_flags.sexp" link_flags)
