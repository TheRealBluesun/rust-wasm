(module binary
  "\00\61\73\6d\01\00\00\00\00\24\10\61\20\63\75\73"
  "\74\6f\6d\20\73\65\63\74\69\6f\6e\74\68\69\73\20"
  "\69\73\20\74\68\65\20\70\61\79\6c\6f\61\64\00\20"
  "\10\61\20\63\75\73\74\6f\6d\20\73\65\63\74\69\6f"
  "\6e\74\68\69\73\20\69\73\20\70\61\79\6c\6f\61\64"
  "\00\11\10\61\20\63\75\73\74\6f\6d\20\73\65\63\74"
  "\69\6f\6e\00\10\00\74\68\69\73\20\69\73\20\70\61"
  "\79\6c\6f\61\64\00\01\00\00\24\10\00\00\63\75\73"
  "\74\6f\6d\20\73\65\63\74\69\6f\00\74\68\69\73\20"
  "\69\73\20\74\68\65\20\70\61\79\6c\6f\61\64\00\24"
  "\10\ef\bb\bf\61\20\63\75\73\74\6f\6d\20\73\65\63"
  "\74\74\68\69\73\20\69\73\20\74\68\65\20\70\61\79"
  "\6c\6f\61\64\00\24\10\61\20\63\75\73\74\6f\6d\20"
  "\73\65\63\74\e2\8c\a3\74\68\69\73\20\69\73\20\74"
  "\68\65\20\70\61\79\6c\6f\61\64\00\1f\16\6d\6f\64"
  "\75\6c\65\20\77\69\74\68\69\6e\20\61\20\6d\6f\64"
  "\75\6c\65\00\61\73\6d\01\00\00\00"
)
(module binary
  "\00\61\73\6d\01\00\00\00\00\0e\06\63\75\73\74\6f"
  "\6d\70\61\79\6c\6f\61\64\00\0e\06\63\75\73\74\6f"
  "\6d\70\61\79\6c\6f\61\64\01\01\00\00\0e\06\63\75"
  "\73\74\6f\6d\70\61\79\6c\6f\61\64\00\0e\06\63\75"
  "\73\74\6f\6d\70\61\79\6c\6f\61\64\02\01\00\00\0e"
  "\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64\00\0e"
  "\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64\03\01"
  "\00\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61"
  "\64\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61"
  "\64\04\01\00\00\0e\06\63\75\73\74\6f\6d\70\61\79"
  "\6c\6f\61\64\00\0e\06\63\75\73\74\6f\6d\70\61\79"
  "\6c\6f\61\64\05\01\00\00\0e\06\63\75\73\74\6f\6d"
  "\70\61\79\6c\6f\61\64\00\0e\06\63\75\73\74\6f\6d"
  "\70\61\79\6c\6f\61\64\06\01\00\00\0e\06\63\75\73"
  "\74\6f\6d\70\61\79\6c\6f\61\64\00\0e\06\63\75\73"
  "\74\6f\6d\70\61\79\6c\6f\61\64\07\01\00\00\0e\06"
  "\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64\00\0e\06"
  "\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64\09\01\00"
  "\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64"
  "\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c\6f\61\64"
  "\0a\01\00\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c"
  "\6f\61\64\00\0e\06\63\75\73\74\6f\6d\70\61\79\6c"
  "\6f\61\64\0b\01\00\00\0e\06\63\75\73\74\6f\6d\70"
  "\61\79\6c\6f\61\64\00\0e\06\63\75\73\74\6f\6d\70"
  "\61\79\6c\6f\61\64"
)
(module binary
  "\00\61\73\6d\01\00\00\00\01\07\01\60\02\7f\7f\01"
  "\7f\00\1a\06\63\75\73\74\6f\6d\74\68\69\73\20\69"
  "\73\20\74\68\65\20\70\61\79\6c\6f\61\64\03\02\01"
  "\00\07\0a\01\06\61\64\64\54\77\6f\00\00\0a\09\01"
  "\07\00\20\00\20\01\6a\0b\00\1b\07\63\75\73\74\6f"
  "\6d\32\74\68\69\73\20\69\73\20\74\68\65\20\70\61"
  "\79\6c\6f\61\64"
)
(assert_malformed
  (module binary "\00\61\73\6d\01\00\00\00\00")
  "unexpected end"
)
(assert_malformed
  (module binary "\00\61\73\6d\01\00\00\00\00\00")
  "unexpected end"
)
(assert_malformed
  (module binary "\00\61\73\6d\01\00\00\00\00\00\00\05\01\00\07\00" "\00")
  "unexpected end"
)
(assert_malformed
  (module binary
    "\00\61\73\6d\01\00\00\00\00\26\10\61\20\63\75\73"
    "\74\6f\6d\20\73\65\63\74\69\6f\6e\74\68\69\73\20"
    "\69\73\20\74\68\65\20\70\61\79\6c\6f\61\64"
  )
  "unexpected end"
)
(assert_malformed
  (module binary
    "\00\61\73\6d\01\00\00\00\00\25\10\61\20\63\75\73"
    "\74\6f\6d\20\73\65\63\74\69\6f\6e\74\68\69\73\20"
    "\69\73\20\74\68\65\20\70\61\79\6c\6f\61\64\00\24"
    "\10\61\20\63\75\73\74\6f\6d\20\73\65\63\74\69\6f"
    "\6e\74\68\69\73\20\69\73\20\74\68\65\20\70\61\79"
    "\6c\6f\61\64"
  )
  "invalid section id"
)
(assert_malformed
  (module binary
    "\00\61\73\6d\01\00\00\00\01\07\01\60\02\7f\7f\01"
    "\7f\00\25\10\61\20\63\75\73\74\6f\6d\20\73\65\63"
    "\74\69\6f\6e\74\68\69\73\20\69\73\20\74\68\65\20"
    "\70\61\79\6c\6f\61\64\03\02\01\00\0a\09\01\07\00"
    "\20\00\20\01\6a\0b\00\1b\07\63\75\73\74\6f\6d\32"
    "\74\68\69\73\20\69\73\20\74\68\65\20\70\61\79\6c"
    "\6f\61\64"
  )
  "function and code section have inconsistent lengths"
)
(assert_malformed
  (module binary "\00\61\73\6d\01\00\00\00\00\61\73\6d\01\00\00\00")
  "length out of bounds"
)