
bootstrap = require("bootstrap")

bootstrap.init()

oasis = require("oasis")
darcs = require("darcs")
ci = require("ci")
godi = require("godi")

ci.init()
godi.init()
oasis.init()
darcs.init()

godi.bootstrap("3.12")
godi.update()
godi.upgrade()
godi.build("godi-findlib")
godi.build("godi-camomile")
godi.build("godi-ocaml-fileutils")
godi.build("godi-ounit")

ci.exec("autoconf")
ci.exec("./configure", "--disable-doc", "--enable-test")
ci.exec("make", "all")
ci.exec("make", "test")
