open Pcaml;;
	EXTEND
	expr: LEVEL "simple"
		[ LEFTA [ "_" ; s = STRING -> <:expr< (Camlgettext.gettext $str:s$) >>]];
	END;;
