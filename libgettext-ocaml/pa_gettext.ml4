open Pcaml;;
	EXTEND
	expr: LEVEL "simple"
		[ LEFTA [ "_" ; s = STRING -> <:expr< (GettextStub.gettext $str:s$) >>]];
	END;;
