# biberon -- a citation formatter

I hand maintain some .bib files as plain-text and need to convert them to markdown citations.

Note that this assumes my own stripped down version of BibTeX, valid .bib files may be rejected but i'll try to make sure my own .bib file will always be accepted by other tools. i.e. my dialect should be a strict subset of BibTeX.

## my dialect

- string values must use `"..."` delimiters. `{...}` is not supported for values. this keeps lexing and parsing cleanly separate: `{` and `}` are always structural tokens delimiting entries, never value delimiters. a lexer that supported both would need to track context to disambiguate, which is parsing work.
- nothing is allowed between entries. in standard BibTeX, any text between entries is ignored and serves as comment. in my dialect, random stuff in between entries is a parse error.
- field lists have a mandated trailing comma. it just makes my job easier. maybe i'll revise at some point but for now i do not care
- full names are required in the list of authors. they are expected to have the form `Last, First Middle`. list of author is separated by `and` always. so `author1 and author2 and author3 and author4`. this is intended to i) be easily parsable and ii) defer the formatting to this tool rather than at .bib input.
- probably other stuff that i'll document later
