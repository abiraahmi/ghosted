# Read a Word file and redact it (no data.frame)

Reads a `.docx` file as raw paragraphs, applies in-function redaction,
and writes a redacted Word file. No speaker/text data.frame is created;
the document is treated as a sequence of paragraphs.

## Usage

``` r
ghost_docx(
  filepath,
  interviewers,
  interviewees = character(),
  output_path = NULL,
  redact_other = character(),
  redact_interviewer = FALSE,
  include_common_names = FALSE,
  common_names_fun = NULL,
  report_redacted = FALSE,
  name_token = "[REDACTED]",
  school_token = "[REDACTED]"
)
```

## Arguments

- filepath:

  Path to a `.docx` file.

- interviewers:

  Character vector of interviewer names.

- interviewees:

  Character vector of interviewee/participant names.

- output_path:

  Path for the redacted `.docx`. If `NULL` (default), set to the same
  directory and base name as `filepath` with `_redacted` before the
  extension (e.g. `report.docx` -\> `report_redacted.docx`).

- redact_other:

  Other words/phrases to redact.

- redact_interviewer:

  If `TRUE`, also redact interviewer names.

- include_common_names:

  If `TRUE`, also redact top US baby names (uses `common_names_fun`).

- common_names_fun:

  Function used when `include_common_names = TRUE` (default: top 1000 US
  baby names from `babynames`).

- report_redacted:

  If `TRUE`, print to the R console which phrases were found and
  redacted (names and other).

- name_token:

  Replacement token for names.

- school_token:

  Replacement token for schools/other phrases.

## Value

The path to the written `.docx` file (invisibly).

## Examples

``` r
# Writes report_redacted.docx in same folder, returns path:
# ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe")
# With common names and redaction report:
# ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   include_common_names = TRUE, report_redacted = TRUE)
```
