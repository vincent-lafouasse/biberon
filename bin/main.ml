[@@@warning "-69-34-37"]

type common_fields =
    {
        author: string;
        title: string;
        year: int;
        archive: string;
    }

type month = int

type article_fields =
    {
        journal: string;
        volume: int;
        pages: int*int;
        number: int;
        month: month;
        doi: string;
    }

type inproceedings_fields =
    {
        booktitle: string;
        pages: int*int;
        doi: string;
    }

type misc_fields = (string * string) list

type bib_entry =
    | Article of common_fields * article_fields
    | Inproceedings of common_fields * inproceedings_fields
    | Misc of common_fields * misc_fields

let foo =
    {
        author = "hello";
        title = "world";
        year = 67;
        archive = "google.com";
    }

let () = print_endline foo.author
