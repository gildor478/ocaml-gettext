open GettextTypes;;

let format_translation_check_data =
  [
    (* Identity *)
    Singular("%d" ,"%d" ),Singular("%d" ,"%d" );
    Singular("%i" ,"%i" ),Singular("%i" ,"%i" );
    Singular("%n" ,"%n" ),Singular("%n" ,"%n" );
    Singular("%N" ,"%N" ),Singular("%N" ,"%N" );
    Singular("%u" ,"%u" ),Singular("%u" ,"%u" );
    Singular("%x" ,"%x" ),Singular("%x" ,"%x" );
    Singular("%X" ,"%X" ),Singular("%X" ,"%X" );
    Singular("%o" ,"%o" ),Singular("%o" ,"%o" );
    Singular("%s" ,"%s" ),Singular("%s" ,"%s" );
    Singular("%S" ,"%S" ),Singular("%S" ,"%S" );
    Singular("%c" ,"%c" ),Singular("%c" ,"%c" );
    Singular("%C" ,"%C" ),Singular("%C" ,"%C" );
    Singular("%f" ,"%f" ),Singular("%f" ,"%f" );
    Singular("%F" ,"%F" ),Singular("%F" ,"%F" );
    Singular("%e" ,"%e" ),Singular("%e" ,"%e" );
    Singular("%E" ,"%E" ),Singular("%E" ,"%E" );
    Singular("%g" ,"%g" ),Singular("%g" ,"%g" );
    Singular("%G" ,"%G" ),Singular("%G" ,"%G" );
    Singular("%B" ,"%B" ),Singular("%B" ,"%B" );
    Singular("%b" ,"%b" ),Singular("%b" ,"%b" );
    Singular("%ld","%ld"),Singular("%ld","%ld");
    Singular("%li","%li"),Singular("%li","%li");
    Singular("%lu","%lu"),Singular("%lu","%lu");
    Singular("%lx","%lx"),Singular("%lx","%lx");
    Singular("%lX","%lX"),Singular("%lX","%lX");
    Singular("%lo","%lo"),Singular("%lo","%lo");
    Singular("%nd","%nd"),Singular("%nd","%nd");
    Singular("%ni","%ni"),Singular("%ni","%ni");
    Singular("%nu","%nu"),Singular("%nu","%nu");
    Singular("%nx","%nx"),Singular("%nx","%nx");
    Singular("%nX","%nX"),Singular("%nX","%nX");
    Singular("%no","%no"),Singular("%no","%no");
    Singular("%Ld","%Ld"),Singular("%Ld","%Ld");
    Singular("%Li","%Li"),Singular("%Li","%Li");
    Singular("%Lu","%Lu"),Singular("%Lu","%Lu");
    Singular("%Lx","%Lx"),Singular("%Lx","%Lx");
    Singular("%LX","%LX"),Singular("%LX","%LX");
    Singular("%Lo","%Lo"),Singular("%Lo","%Lo");
    Singular("%a" ,"%a" ),Singular("%a" ,"%a" );
    Singular("%t" ,"%t" ),Singular("%t" ,"%t" );
    Singular("%!" ,"%!" ),Singular("%!" ,"%!" );
    Singular("%%" ,"%%" ),Singular("%%" ,"%%" );

    (* Always fails *)
    Singular("%d" ,"" ),Singular("%d" ,"%d" );
    Singular("%i" ,"" ),Singular("%i" ,"%i" );
    Singular("%n" ,"" ),Singular("%n" ,"%n" );
    Singular("%N" ,"" ),Singular("%N" ,"%N" );
    Singular("%u" ,"" ),Singular("%u" ,"%u" );
    Singular("%x" ,"" ),Singular("%x" ,"%x" );
    Singular("%X" ,"" ),Singular("%X" ,"%X" );
    Singular("%o" ,"" ),Singular("%o" ,"%o" );
    Singular("%s" ,"" ),Singular("%s" ,"%s" );
    Singular("%S" ,"" ),Singular("%S" ,"%S" );
    Singular("%c" ,"" ),Singular("%c" ,"%c" );
    Singular("%C" ,"" ),Singular("%C" ,"%C" );
    Singular("%f" ,"" ),Singular("%f" ,"%f" );
    Singular("%F" ,"" ),Singular("%F" ,"%F" );
    Singular("%e" ,"" ),Singular("%e" ,"%e" );
    Singular("%E" ,"" ),Singular("%E" ,"%E" );
    Singular("%g" ,"" ),Singular("%g" ,"%g" );
    Singular("%G" ,"" ),Singular("%G" ,"%G" );
    Singular("%B" ,"" ),Singular("%B" ,"%B" );
    Singular("%b" ,"" ),Singular("%b" ,"%b" );
    Singular("%ld","" ),Singular("%ld","%ld");
    Singular("%li","" ),Singular("%li","%li");
    Singular("%lu","" ),Singular("%lu","%lu");
    Singular("%lx","" ),Singular("%lx","%lx");
    Singular("%lX","" ),Singular("%lX","%lX");
    Singular("%lo","" ),Singular("%lo","%lo");
    Singular("%nd","" ),Singular("%nd","%nd");
    Singular("%ni","" ),Singular("%ni","%ni");
    Singular("%nu","" ),Singular("%nu","%nu");
    Singular("%nx","" ),Singular("%nx","%nx");
    Singular("%nX","" ),Singular("%nX","%nX");
    Singular("%no","" ),Singular("%no","%no");
    Singular("%Ld","" ),Singular("%Ld","%Ld");
    Singular("%Li","" ),Singular("%Li","%Li");
    Singular("%Lu","" ),Singular("%Lu","%Lu");
    Singular("%Lx","" ),Singular("%Lx","%Lx");
    Singular("%LX","" ),Singular("%LX","%LX");
    Singular("%Lo","" ),Singular("%Lo","%Lo");
    Singular("%a" ,"" ),Singular("%a" ,"%a" );
    Singular("%t" ,"" ),Singular("%t" ,"%t" );

    (* Mismatch *)
    Singular("%d" ,"%i" ),Singular("%d" ,"%d" );
    Singular("%i" ,"%d" ),Singular("%i" ,"%i" );
    Singular("%n" ,"%d" ),Singular("%n" ,"%n" );
    Singular("%N" ,"%d" ),Singular("%N" ,"%N" );
    Singular("%u" ,"%d" ),Singular("%u" ,"%u" );
    Singular("%x" ,"%d" ),Singular("%x" ,"%x" );
    Singular("%X" ,"%d" ),Singular("%X" ,"%X" );
    Singular("%o" ,"%d" ),Singular("%o" ,"%o" );
    Singular("%s" ,"%d" ),Singular("%s" ,"%s" );
    Singular("%S" ,"%d" ),Singular("%S" ,"%S" );
    Singular("%c" ,"%d" ),Singular("%c" ,"%c" );
    Singular("%C" ,"%d" ),Singular("%C" ,"%C" );
    Singular("%f" ,"%d" ),Singular("%f" ,"%f" );
    Singular("%F" ,"%d" ),Singular("%F" ,"%F" );
    Singular("%e" ,"%d" ),Singular("%e" ,"%e" );
    Singular("%E" ,"%d" ),Singular("%E" ,"%E" );
    Singular("%g" ,"%d" ),Singular("%g" ,"%g" );
    Singular("%G" ,"%d" ),Singular("%G" ,"%G" );
    Singular("%B" ,"%d" ),Singular("%B" ,"%B" );
    Singular("%b" ,"%d" ),Singular("%b" ,"%b" );
    Singular("%ld","%d" ),Singular("%ld","%ld");
    Singular("%li","%d" ),Singular("%li","%li");
    Singular("%lu","%d" ),Singular("%lu","%lu");
    Singular("%lx","%d" ),Singular("%lx","%lx");
    Singular("%lX","%d" ),Singular("%lX","%lX");
    Singular("%lo","%d" ),Singular("%lo","%lo");
    Singular("%nd","%d" ),Singular("%nd","%nd");
    Singular("%ni","%d" ),Singular("%ni","%ni");
    Singular("%nu","%d" ),Singular("%nu","%nu");
    Singular("%nx","%d" ),Singular("%nx","%nx");
    Singular("%nX","%d" ),Singular("%nX","%nX");
    Singular("%no","%d" ),Singular("%no","%no");
    Singular("%Ld","%d" ),Singular("%Ld","%Ld");
    Singular("%Li","%d" ),Singular("%Li","%Li");
    Singular("%Lu","%d" ),Singular("%Lu","%Lu");
    Singular("%Lx","%d" ),Singular("%Lx","%Lx");
    Singular("%LX","%d" ),Singular("%LX","%LX");
    Singular("%Lo","%d" ),Singular("%Lo","%Lo");
    Singular("%a" ,"%d" ),Singular("%a" ,"%a" );
    Singular("%t" ,"%d" ),Singular("%t" ,"%t" );
    Singular("%!" ,"%d" ),Singular("%!" ,"%!" );
    Singular("%%" ,"%d" ),Singular("%%" ,"%%" );

    (* All in one *)
    Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%", 
    "%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%"),
    Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%", 
    "%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%");
    Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%", 
    "%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E g %G %B %b %ld %li %lu %lx %lX "
    ^"lo nd ni nu nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%"),
    Singular("%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%", 
    "%d %i %n %N %u %x %X %o %s %S %c %C "
    ^"%f %F %e %E %g %G %B %b %ld %li %lu %lx %lX "
    ^"%lo %nd %ni %nu %nx %nX %no %Ld %Li %Lu %Lx "
    ^"%LX %Lo %a %t %! %%");

    (* Plural forms *)
    Plural("singular %d","plural %i",["%d";"%d"]),Plural("singular %d","singular %d",["%d";"%d"]);
    Plural("singular %d","plural %d",["%i";"%d"]),Plural("singular %d","plural %d",["singular %d";"%d"]);
    Plural("singular %d","plural %d",["%d";"%i"]),Plural("singular %d","plural %d",["%d";"plural %d"]);
    Plural("singular %d","plural %d",["%d";"%d"]),Plural("singular %d","plural %d",["%d";"%d"]);

    (* Idem potent *)
    Singular("%%",""),Singular("%%","");
    Singular("%!",""),Singular("%!","");
    Singular("",""),Singular("","");
    Singular("a","b"),Singular("a","b");
  ]
;;

let format_translation_all_data = 
  List.fold_left ( fun lst (a,b) -> a :: b :: lst )
  [] format_translation_check_data
;;

let format_translation_plural_data =
  List.filter ( function Plural(_,_,_) -> true | _ -> false )
  format_translation_all_data
;;

let format_translation_singular_data = 
  List.filter ( function Singular(_,_) -> true | _ -> false )
  format_translation_all_data
;;
