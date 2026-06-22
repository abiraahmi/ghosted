# Redact transcripts using a local app

Opens a local Shiny app that asks for an input directory, output
directory, known interviewer names, known participant names, and other
terms to redact. The app scans supported transcript files in the input
directory, shows likely names for review, and writes redacted transcript
files. Supported input types are `.docx`, `.txt`, and `.vtt`.

## Usage

``` r
ghost(
  input_dir = NULL,
  output_dir = NULL,
  interviewers = character(),
  interviewees = character(),
  redact_other = character(),
  redact_interviewer = TRUE,
  include_common_names = FALSE,
  redacted_token = "[REDACTED]",
  add_blank_line_between_turns = TRUE,
  recursive = FALSE,
  suffix = "_redacted",
  out_format = NULL,
  report_redacted = FALSE,
  name_review_min_score = 2,
  show_completion_notice = TRUE
)
```

## Arguments

- input_dir:

  Optional folder containing transcripts. If `NULL`, the app asks for
  it.

- output_dir:

  Optional folder to write outputs. If `NULL`, the app asks for it,
  defaulting to `input_dir` when left blank.

- interviewers:

  Optional character vector of interviewer names to prefill in the app.

- interviewees:

  Optional character vector of interviewee/participant names to prefill
  in the app.

- redact_other:

  Optional terms to prefill in the app's other-redaction field.

- redact_interviewer:

  Default value for the app option that controls whether interviewer
  names are redacted in body text.

- include_common_names:

  Default value for the app option that includes the package's default
  common-name list when available.

- redacted_token:

  Default replacement token shown in the app.

- add_blank_line_between_turns:

  Default value for the app option that inserts a blank line between
  turns for DOCX/TXT outputs.

- recursive:

  Default value for the app option to include subdirectories.

- suffix:

  Default suffix shown in the app for output filenames.

- out_format:

  Default output format shown in the app. One of `"vtt"`, `"docx"`, or
  `"txt"`. If `NULL`, the app defaults to keeping original formats.

- report_redacted:

  Default value for the app option that prints redaction summaries to
  the R console.

- name_review_min_score:

  Minimum rule-based score for a candidate to appear in the review app.

- show_completion_notice:

  Default value for the app option that opens a local completion notice
  after all outputs are written.

## Value

A data.frame with `input_file`, `output_file`, `status`, and redaction
report columns. Invisibly returned.

## Examples

``` r
# In an interactive R session:
# ghost()
```
