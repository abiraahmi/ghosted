# Read a Word file and redact it (no data.frame)

Reads a `.docx` file as raw paragraphs, applies in-function redaction,
and writes a redacted file. No speaker/text data.frame is created; the
document is treated as a sequence of paragraphs. You can choose the
output format (DOCX/TXT/VTT) via `out_format` similar to
[`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md)
and
[`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md).

## Usage

``` r
ghost_docx(
  filepath,
  interviewers,
  interviewees = character(),
  redact_other = character(),
  redact_interviewer = TRUE,
  include_common_names = FALSE,
  redacted_token = "[REDACTED]",
  add_blank_line_between_turns = TRUE,
  output_path = NULL,
  suffix = "_redacted",
  out_format = c("docx", "txt", "vtt"),
  report_redacted = FALSE
)
```

## Arguments

- filepath:

  Path to a `.docx` file.

- interviewers:

  Character vector of interviewer names.

- interviewees:

  Character vector of interviewee/participant names.

- redact_other:

  Other words/phrases to redact.

- redact_interviewer:

  If `TRUE` (default), redact interviewer names in body text. If
  `FALSE`, interviewer names are preserved in body text; leading speaker
  labels are still normalized to `Interviewer` regardless.

- include_common_names:

  If `TRUE`, also redact a default list of common names (e.g., top US
  baby names, if available via `ghosted::common_names_default`). Emits a
  warning when the dataset is not bundled in the installed version.

- redacted_token:

  Replacement token used for redactions (names and other phrases).

- add_blank_line_between_turns:

  Logical; for TXT/DOCX outputs when converting formats, insert a blank
  line between turns. This does not affect DOCX→DOCX.

- output_path:

  Path for the redacted file. If `NULL` (default), set to the same
  directory and base name as `filepath` with `_redacted` before the
  extension. The extension is chosen based on `out_format` (e.g.
  `report.docx` -\> `report_redacted.docx` for `out_format = "docx"`, or
  `report_redacted.txt` / `report_redacted.vtt` otherwise).

- suffix:

  Suffix to append to the base filename (default: `"_redacted"`). Only
  used when `output_path` is `NULL`.

- out_format:

  One of `"docx"`, `"txt"`, or `"vtt"` controlling the output file type.
  Defaults to `"docx"`.

- report_redacted:

  If `TRUE`, print to the R console which phrases were found and
  redacted (names and other).

## Value

The path to the written output file (invisibly).

## Examples

``` r
# Writes report_redacted.docx in same folder, returns path:
# ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe")
# Write as TXT instead of DOCX:
# ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   out_format = "txt")
# With common names and redaction report:
# ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   include_common_names = TRUE, report_redacted = TRUE)
```
