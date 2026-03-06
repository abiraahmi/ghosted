# Batch redact transcripts in a folder

Scans a folder for transcript files and runs the appropriate redaction
function per file type:

- `.docx` -\>
  [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md)

- `.txt` -\>
  [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md)

- `.vtt` -\>
  [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md)
  (can output VTT/DOCX/TXT)

## Usage

``` r
ghost_batch(
  input_dir,
  interviewers,
  interviewees = character(),
  output_dir = NULL,
  recursive = FALSE,
  suffix = "_redacted",
  out_format = NULL,
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

- input_dir:

  Folder containing transcripts.

- interviewers:

  Character vector of interviewer names.

- interviewees:

  Character vector of interviewee/participant names.

- output_dir:

  Folder to write outputs (default: `input_dir`).

- recursive:

  If `TRUE`, include files in subdirectories.

- suffix:

  Suffix to append to each output filename (default: `_redacted`).

- out_format:

  Output format for all inputs; one of `"vtt"`, `"docx"`, or `"txt"`. If
  `NULL` (default), each file keeps its original format. If you request
  `"vtt"` for `.docx`/`.txt` inputs, cues are written without
  timestamps.

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

  If `TRUE`, print phrases found/redacted per file.

- name_token:

  Replacement token for names.

- school_token:

  Replacement token for schools/other phrases.

- add_blank_line_between_turns:

  For VTT -\> DOCX/TXT outputs, insert a blank line between turns.

## Value

A data.frame with `input_file`, `output_file`, and `status` columns.
Invisibly returned.

## Details

Outputs are written to `output_dir` (defaults to `input_dir`) using the
input base filename with `suffix` appended before the extension. Shows a
console progress bar while processing.

## Examples

``` r
# Redact all transcripts in a folder, keeping the same formats:
# ghost_batch("inst/data/transcripts", interviewers = "Dr. Smith",
#   interviewees = "Jane Doe", suffix = "_DEID")
# Redact VTTs to DOCX, others keep their formats:
# ghost_batch("inst/data/transcripts", interviewers = "Dr. Smith",
#   interviewees = "Jane Doe", out_format = "docx")
```
