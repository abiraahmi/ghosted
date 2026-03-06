# Read a text file and redact it (no data.frame)

Reads a `.txt` file line-by-line, applies in-function redaction, and
writes a redacted file. No speaker/text data.frame is created; the file
is treated as a sequence of lines. You can choose the output format
(TXT/DOCX/VTT) via `out_format` similar to
[`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md)
and
[`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md).

## Usage

``` r
ghost_txt(
  filepath,
  interviewers,
  interviewees = character(),
  redact_other = character(),
  redact_interviewer = FALSE,
  include_common_names = FALSE,
  redacted_token = "[REDACTED]",
  add_blank_line_between_turns = TRUE,
  output_path = NULL,
  suffix = "_redacted",
  out_format = c("txt", "docx", "vtt"),
  report_redacted = FALSE
)
```

## Arguments

- filepath:

  Path to a `.txt` file.

- interviewers:

  Character vector of interviewer names.

- interviewees:

  Character vector of interviewee/participant names.

- redact_other:

  Other words/phrases to redact.

- redact_interviewer:

  If `TRUE`, also redact interviewer names.

- include_common_names:

  If `TRUE`, also redact a default list of common names (e.g., top US
  baby names, if available via `ghosted::common_names_default`).

- redacted_token:

  Replacement token used for redactions (names and other phrases).

- add_blank_line_between_turns:

  Logical; for DOCX outputs, insert a blank line between turns.

- output_path:

  Path for the redacted file. If `NULL` (default), set to the same
  directory and base name as `filepath` with `_redacted` before the
  extension. The extension is chosen based on `out_format` (e.g.
  `notes.txt` -\> `notes_redacted.txt` for `out_format = "txt"`, or
  `notes_redacted.docx` / `notes_redacted.vtt` otherwise).

- suffix:

  Suffix to append to the base filename (default: `"_redacted"`). Only
  used when `output_path` is `NULL`.

- out_format:

  One of `"txt"`, `"docx"`, or `"vtt"` controlling the output file type.
  Defaults to `"txt"`.

- report_redacted:

  If `TRUE`, print to the R console which phrases were found and
  redacted (names and other).

## Value

The path to the written output file (invisibly).

## Examples

``` r
# Writes notes_redacted.txt in same folder, returns path:
# ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
# Write as DOCX instead of TXT:
# ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   out_format = "docx")
# With common names and redaction report:
# ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   include_common_names = TRUE, report_redacted = TRUE)
```
