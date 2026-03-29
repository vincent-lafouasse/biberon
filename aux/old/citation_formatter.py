import bibtexparser
import re

# required fields for _all_documents
COMMON_REQUIRED = {
    "author",
    "title",
    "year",
    "archive",
}

# type-specific, e.g. journal for an article
# mostly IEEE compliant (i skipped the location field for proceedings)
TYPE_SPECIFIC_REQUIRED = {
    "article": {
        "journal",
        "volume",
        "pages",
        "number",  # issue
        "month",
        "doi",
    },
    "inproceedings": {
        "booktitle",
        "pages",
        "doi",
    },
    "misc": set(),  # maybe howpublished later but for now, nothing
}


def validate_entry(entry):
    entry_type = entry.get("ENTRYTYPE").lower()

    fields = set(entry.keys())

    # set operations
    missing_common_fields = COMMON_REQUIRED - fields

    specific_fields = TYPE_SPECIFIC_REQUIRED.get(entry_type)
    if specific_fields is None:
        print(f"Unrecognised entry type: {entry_type}. No required fields")
        specific_fields = set()

    missing_specific_fields = specific_fields - fields

    valid = True

    for missing in missing_common_fields:
        print(f"{entry.get("ID")}: missing common field:\t{missing}")
        valid = False

    for missing in missing_specific_fields:
        print(f"{entry.get("ID")}: missing specific field:\t{missing}")
        valid = False

    if valid:
        valid_str = "ok"
    else:
        valid_str = "ko"
    print(f"---- {entry.get("ID")}: {valid_str}")
    return valid


def validate_library(library):
    invalid_entries = [
        entry.get("ID") for entry in library.entries if not validate_entry(entry)
    ]

    if len(invalid_entries) != 0:
        raise ValueError("Malformed library")


class Markdown:
    @staticmethod
    def bold(text):
        return f"**{text}**" if text else ""

    @staticmethod
    def it(text):
        return f"*{text}*" if text else ""

    @staticmethod
    def link(url, text=None):
        if not text:
            text = url
        return f"[{text}]({url})"

    @staticmethod
    def sup(text):
        return f"^{text}^" if text else ""


class Author:
    def __init__(self, first_names, last_name):
        # first_names is expected to be a list of strings
        self.first_names = first_names
        self.last_name = last_name

    @classmethod
    def parse(cls, name_string):
        name_string = name_string.strip()

        if "," in name_string:
            # Format: Last, First Middle
            parts = name_string.split(",")
            last_name = parts[0].strip()
            first_names = parts[1].strip().split()
        else:
            # Format: First Middle Last
            parts = name_string.split()
            if len(parts) > 1:
                last_name = parts[-1]
                first_names = parts[:-1]
            else:
                last_name = parts[0]
                first_names = []

        return cls(first_names, last_name)

    def jacs(self):
        # Format: Last, F. M.
        initials = " ".join([f"{n[0]}." for n in self.first_names])
        return f"{self.last_name}, {initials}"

    def ieee(self) -> str:
        """Format: F. M. Last"""
        if not self.first_names:
            return self.last_name
        initials = " ".join([f"{n[0]}." for n in self.first_names])
        return f"{initials} {self.last_name}"

    def __repr__(self):
        return f"Author({self.last_name}, {self.first_names})"


class AuthorList:
    def __init__(self, author_list):
        self.author_list = author_list

    @classmethod
    def parse(cls, string):
        author_list = re.split(r"\s+and\s+", string, flags=re.IGNORECASE)
        author_list = [author.strip() for author in author_list]
        author_list = [Author.parse(author) for author in author_list]
        return cls(author_list)

    def ieee(self):
        formatted_names = [a.ieee() for a in self.author_list]
        count = len(formatted_names)

        if count == 0:
            return ""
        if count == 1:
            return formatted_names[0]
        if count == 2:
            return f"{formatted_names[0]} and {formatted_names[1]}"

        # for 3+, use Oxford comma per IEEE
        return ", ".join(formatted_names[:-1]) + f", and {formatted_names[-1]}"


# a double sanity check that the field exists
def get_field(entry, field):
    mnemonic = entry.get("ID")
    out = entry.get(field)
    if out is None or not str(out).strip():
        raise ValueError(f"{mnemonic} has no field {field}")
    return out


# format per IEEE, give it anything _reasonable_
def format_month(month_input: str) -> str:
    """
    possible inputs: '4', '04', 'apr', 'April', 'apr.'
    """
    if not month_input:
        return ""

    clean_input = str(month_input).lower().strip().replace(".", "")

    month_map = {
        "1": "Jan.",
        "01": "Jan.",
        "jan": "Jan.",
        "january": "Jan.",
        "2": "Feb.",
        "02": "Feb.",
        "feb": "Feb.",
        "february": "Feb.",
        "3": "Mar.",
        "03": "Mar.",
        "mar": "Mar.",
        "march": "Mar.",
        "4": "Apr.",
        "04": "Apr.",
        "apr": "Apr.",
        "april": "Apr.",
        "5": "May",
        "05": "May",
        "may": "May",
        "6": "June",
        "06": "June",
        "june": "June",
        "7": "July",
        "07": "July",
        "july": "July",
        "8": "Aug.",
        "08": "Aug.",
        "aug": "Aug.",
        "august": "Aug.",
        "9": "Sept.",
        "09": "Sept.",
        "sep": "Sept.",
        "sept": "Sept.",
        "september": "Sept.",
        "10": "Oct.",
        "oct": "Oct.",
        "october": "Oct.",
        "11": "Nov.",
        "nov": "Nov.",
        "november": "Nov.",
        "12": "Dec.",
        "dec": "Dec.",
        "december": "Dec.",
    }

    return month_map.get(clean_input, month_input.capitalize())


def full_doi(doi_str: str) -> str:
    if not doi_str:
        return ""

    doi = doi_str.strip()

    # Check if it's already a full link
    if doi.startswith("http"):
        return doi

    # Standard IEEE/ACM/Springer resolver prefix
    return f"https://doi.org/{doi}"


def format_entry(entry) -> str:
    mnemonic = get_field(entry, "ID")
    etype = get_field(entry, "ENTRYTYPE").lower()

    authors = AuthorList.parse(get_field(entry, "author"))
    title = get_field(entry, "title").strip("{}")
    year = get_field(entry, "year")
    archive = get_field(entry, "archive")

    header = f'\\[{mnemonic}\\] {authors.ieee()}, "{title},"'

    archive_link = Markdown.link(archive, "Archive")
    doi = entry.get("doi")
    if doi:
        doi_url = full_doi(doi)
        doi_link = Markdown.link(doi_url, doi)
        footer = f"{year}. DOI: {doi_link}. {archive_link}."
    else:
        footer = f"{year}. {archive_link}."

    if etype == "article":
        jrnl = Markdown.it(get_field(entry, "journal"))
        vol = get_field(entry, "volume")
        num = get_field(entry, "number")
        pages = get_field(entry, "pages").replace("--", "–")
        month = format_month(get_field(entry, "month"))
        middle = f" {jrnl}, vol. {vol}, no. {num}, pp. {pages}, {month} "
    elif etype == "inproceedings":
        # Using series as an optional bonus if present, otherwise booktitle only
        venue = get_field(entry, "booktitle").replace("Proceedings of the", "Proc.")
        if not venue.startswith("Proc."):
            venue = f"Proc. {venue}"
        series = entry.get("series")
        venue_str = f"{venue} ({series})" if series else venue
        pages = (
            get_field(entry, "pages")
            .replace("–", "-")
            .replace("--", "-")
            .replace("-", "–")
        )
        middle = f" in {Markdown.it(venue_str)}, pp. {pages}, "
    else:
        middle = " "

    return f"{header}{middle}{footer}"


def main():
    with open("references.bib") as bibtex_file:
        library = bibtexparser.load(bibtex_file)

    validate_library(library)

    for entry in library.entries:
        print(format_entry(entry))
        print()


if __name__ == "__main__":
    main()
