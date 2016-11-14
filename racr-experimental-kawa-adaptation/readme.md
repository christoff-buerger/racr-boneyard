# Experimental *Kawa* adaptation of *RACR*

The `core.scm` contained in this *zip* archive is an **experimental** adaptation of *RACR's* `(racr core)` library for [*Kawa*](https://www.gnu.org/software/kawa/). It is compiling with *Kawa 2.2*, the most recent version on the 15th of November 2016.

The adaptations done are:
 * transformation of *R6RS* library syntax to *R7RS/Kawa* syntax
 * transformation of *R6RS* record definitions to *SRFI 9* records (as supported by *Kawa*)
 * split of `specify-ag-rule` and separation of its nested `specify-attribute*` macro in an own definition (*Kawa's* `define-syntax` implementation is incomplete)
 * deletion of everything related to pattern attributes (to reduce necessary adaptations)
 * addition of imports (`(scheme base)`, `(scheme case-lambda)`, `(scheme char)`, `(scheme write)`, `(rnrs lists)`, `(rnrs hashtables)` and `(kawa base)`)
 * renaming of `call-with-string-output-port` to `call-with-output-string`
 * deletion of `racr-exception` and replacement of its applications by `error` (`define-condition-type` not supported by *Kawa*)

