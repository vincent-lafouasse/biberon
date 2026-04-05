# biberon -- a citation formatter

I hand maintain some .bib files as plain-text and need to convert them to markdown citations.

Note that this assumes my own stripped down version of BibTeX, valid .bib files may be rejected but i'll try to make sure my own .bib file will always be accepted by other tools. i.e. my dialect should be a strict subset of BibTeX.

## my dialect

- string values must use `"..."` delimiters. `{...}` is not supported for values. this keeps lexing and parsing cleanly separate: `{` and `}` are always structural tokens delimiting entries, never value delimiters. a lexer that supported both would need to track context to disambiguate, which is parsing work.
- nothing is allowed between entries. in standard BibTeX, any text between entries is ignored and serves as comment. in my dialect, random stuff in between entries is a parse error.
- field lists have a mandated trailing comma. it just makes my job easier. maybe i'll revise at some point but for now i do not care
- full names are required in the list of authors. they are expected to have the form `Last, First Middle`. list of author is separated by `and` always. so `author1 and author2 and author3 and author4`. this is intended to i) be easily parsable and ii) defer the formatting to this tool rather than at .bib input.
- months are required to use the standard BibTeX three-letter lowercase form `jan feb mar apr may jun jul aug sep oct nov dec`. i'm not down to parse arbitrary formats
- no duplicate fields. i'm ignoring non-required fields so you can still put any field you want, just don't repeat them
- probably other stuff that i'll document later

## citation style

in a loosely compliant IEEE style. i'm not interested in being legally compliant (i'd use a tool for that if it matters that much), i just want the citations to look the way they should.

styles i'm considering post 1.0 are ACS (chemistry nostalgia), ACM (if ocaml lights a spark in me), AIP/APS (for physics, maybe even ASA). maybe even APA, MLA, Nature if i want to cover common mainstream styles as well.

### IEEE format

authors are listed as initials followed by last name, separated by commas, with "and" before the last: `J. Doe, A. Smith, and B. Jones`. for seven or more authors, the first is listed followed by `et al.`

the general shape of an article citation is:

A. Author, "Title of the paper", *Journal Name*, vol. X, no. Y, pp. start--end, Mon. YYYY, doi: 10.XXXX/YYYY.

and for inproceedings:

A. Author, "Title of the paper", in *Proceedings of the Conference*, Location, YYYY, pp. start--end, doi: 10.XXXX/YYYY.

details:
- article/paper titles are in sentence case and wrapped in double quotes, comma outside the closing quote
- journal and conference names are in title case and italicised
- inproceedings use `in` before the conference name
- volume as `vol. X`, issue/number as `no. Y`, pages as `pp. start--end`
- month is abbreviated to three letters: Jan., Feb., Mar., etc.
- DOI formatted as `doi: 10.XXXX/YYYY` at the end
- entries end with a period

## example

from my concurrent lock free queue review:

A. Swartz, "Guerilla Open Access Manifesto", misc, 2008, [Archive](https://archive.org/details/GuerillaOpenAccessManifesto).

L. Lamport, "Specifying concurrent program modules", *ACM Trans. Program. Lang. Syst.*, vol. 5, no. 2, pp. 190--222, Apr. 1983, doi: [10.1145/69624.357207](https://doi.org/10.1145/69624.357207), [Archive](https://lamport.azurewebsites.net/pubs/spec.pdf).

M. M. Michael, and M. L. Scott, "Simple, fast, and practical non-blocking and blocking concurrent queue algorithms", in *15th Annu. ACM Symp. Princ. Distrib. Comput.*, 1996, pp. 267--275, doi: [10.1145/248052.248106](https://doi.org/10.1145/248052.248106), [Archive](https://www.cs.rochester.edu/u/scott/papers/1996_PODC_queues.pdf).

E. Ladan-Mozes, and N. Shavit, "An optimistic approach to lock-free FIFO queues", in *18th Int. Symp. Distrib. Comput.*, 2004, pp. 117--131, doi: [10.1007/978-3-540-30186-8_9](https://doi.org/10.1007/978-3-540-30186-8_9), [Archive](https://people.csail.mit.edu/shanir/publications/FIFO_Queues.pdf).

J. Giacomoni, T. Moseley, and M. Vachharajani, "FastForward For efficient pipeline parallelism: a cache-optimized concurrent lock-free queue", in *13th ACM SIGPLAN Symp. Princ. Pract. Parallel Program.*, 2008, pp. 43--52, doi: [10.1145/1345206.1345215](https://doi.org/10.1145/1345206.1345215), [Archive](https://dl.acm.org/doi/epdf/10.1145/1345206.1345215).


M. Aldinucci, M. Danelutto, P. Kilpatrick, M. Meneghin, and M. Torquati, "An efficient unbounded lock-free queue for multi-core systems", in *18th Int. Conf. Parallel Process.*, 2012, pp. 662--673, doi: [10.1007/978-3-642-32820-6_65](https://doi.org/10.1007/978-3-642-32820-6_65), [Archive](https://link.springer.com/content/pdf/10.1007/978-3-642-32820-6_65.pdf).

N. M. Lê, A. Guatto, A. Cohen, and A. Pop, "Correct and efficient bounded FIFO queues", in *25th Int. Symp. Comp. Archit. High Perform. Comput.*, 2013, pp. 144--151, doi: [10.1109/SBAC-PAD.2013.8](https://doi.org/10.1109/SBAC-PAD.2013.8), [Archive](https://www.irif.fr/~guatto/publications/sbac13.pdf).

## ideas for later

- maybe allow some `@mapping` entries for text substitution, because no-one wants to write "IEEE International Conference on Acoustics, Speech and Signal Processing" five times in a row
- or maybe those mappings could be passed from an optionally passed spec file, not sure
- cover more citation styles
