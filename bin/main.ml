[@@@warning "-69"]

type entry =
    {
        author: string;
        title: string;
        year: int;
        archive: string;
    }

let foo =
    {
        author = "hello";
        title = "world";
        year = 67;
        archive = "google.com";
    }

let () = print_endline foo.author
