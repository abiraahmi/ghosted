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
  output_dir = NULL,
  out_format = c("vtt", "docx", "txt"),
  suffix = "_redacted",
  redact_other = character(),
  redact_interviewer = FALSE,
  include_common_names = FALSE,
  common_names_fun = NULL,
  report_redacted = FALSE,
  name_token = "[REDACTED]",
  school_token = "[REDACTED]",
  add_blank_line_between_turns = TRUE
)
```

## Arguments

- filepath:

  Path to a `.vtt` file.

- interviewers:

  Character vector of interviewer names.

- interviewees:

  Character vector of interviewee/participant names.

- output_dir:

  Output folder. If `NULL`, uses the folder of `filepath`. The output
  filename is derived from the input base name with `suffix` and an
  extension based on `out_format`.

- out_format:

  One of `"vtt"`, `"docx"`, or `"txt"` controlling the output file
  extension.

- suffix:

  Suffix to append to the base filename (default: `"_redacted"`).

- redact_other:

  Other words/phrases to redact.

- redact_interviewer:

  If `TRUE`, also redact interviewer names in the transcript text.
  Interviewer names in the speaker field are always redacted.

- include_common_names:

  If `TRUE`, also redact top US baby names (uses `common_names_fun`).

- common_names_fun:

  Function used when `include_common_names = TRUE` (default: top 1000 US
  baby names from `babynames`).

- report_redacted:

  If `TRUE`, prints which phrases were found and redacted.

- name_token:

  Replacement token for names.

- school_token:

  Replacement token for schools/other phrases.

- add_blank_line_between_turns:

  Logical; for DOCX/TXT outputs, insert a blank line between turns.

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
