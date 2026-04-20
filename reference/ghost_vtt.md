# Redact a Zoom/WebVTT transcript and write VTT/DOCX/TXT

Parses a Zoom/WebVTT transcript as raw cues (no data.frame), redacts
interviewee names (and optionally interviewer names) plus other phrases
using boundary-aware matching, and writes the result as a WebVTT, Word,
or plain text file.

## Usage

``` r
ghost_vtt(
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
  out_format = c("vtt", "docx", "txt"),
  report_redacted = FALSE
)
```

## Arguments

- filepath:

  Path to a `.vtt` file.

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

  Logical; for DOCX/TXT outputs, insert a blank line between turns.

- output_path:

  Full path for the output file. If `NULL`, uses the folder of
  `filepath` with the input base name plus `suffix` and an extension
  based on `out_format`.

- suffix:

  Suffix to append to the base filename (default: `"_redacted"`).

- out_format:

  One of `"vtt"`, `"docx"`, or `"txt"` controlling the output file
  extension.

- report_redacted:

  If `TRUE`, prints which phrases were found and redacted.

## Value

Invisibly, the output path written.

## Details

Redaction mirrors the standalone logic used in
[`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md)
and
[`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md):
full names are also split into parts (e.g., first/last and hyphenated
pieces) and replaced longest-first with tokens.

## Examples

``` r
# Write redacted VTT next to source:
# ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
# Write redacted DOCX with report:
# ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#   out_format = "docx", report_redacted = TRUE)
```
