# biberon -- a citation formatter

I hand maintain some .bib files as plain-text and need to convert them to markdown citations.

Note that this assumes my own stripped down version of BibTeX, valid .bib files may be rejected.

## my dialect

- string values must use `"..."` delimiters. `{...}` is not supported for values. this keeps lexing and parsing cleanly separate: `{` and `}` are always structural tokens delimiting entries, never value delimiters. a lexer that supported both would need to track context to disambiguate, which is parsing work.
- nothing is allowed between entries. in standard BibTeX, any text between entries is ignored and serves as comment. in my dialect, random stuff in between entries is a parse error.
- probably other stuff that i'll document later
