
<div style="text-align: center; margin-bottom: 2em;">

# ghosted

<!-- badges: start -->

<!-- badges: end -->

</div>

De-identify transcripts with helpers:

- `ghost_vtt()` — redact a `.vtt` (Zoom/WebVTT) file and write
  `.vtt`/`.docx`/`.txt`; supports `redact_other`, `redact_interviewer`,
  `include_common_names`, `redacted_token`, `add_blank_line_between_turns`,
  `output_path`, `suffix`, `out_format`, `report_redacted`.
- `ghost_docx()` — redact a `.docx` and write `.docx`/`.txt`/`.vtt`;
  supports `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_path`, `suffix`,
  `out_format`, `report_redacted`.
- `ghost_txt()` — redact a `.txt` and write `.txt`/`.docx`/`.vtt`;
  supports `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_path`, `suffix`,
  `out_format`, `report_redacted`.
- `ghost_batch()` — run the same logic across a folder of
  `.vtt`/`.docx`/`.txt` files; supports `redact_other`, `redact_interviewer`,
  `include_common_names`, `redacted_token`, `add_blank_line_between_turns`,
  `output_dir`, `suffix`, `out_format`, `report_redacted`.

Key options (common across functions):

- `redact_other`: extra phrases to redact (in addition to names).
- `redact_interviewer`: also redact interviewer names in text.
- `include_common_names`: add a default list of common names to redact.
- `redacted_token`: replacement token used for redactions (default `[REDACTED]`).
- `add_blank_line_between_turns`: for DOCX/TXT outputs, insert a blank line between turns.
- `output_path` (single-file functions) or `output_dir` (ghost_batch): where to write results.
- `suffix`: appended to output base name when `output_path`/`output_dir` is used
  (default `"_redacted"`).
- `out_format`: choose output type: `"vtt"`, `"docx"`, or `"txt"`.
- `report_redacted`: print which phrases were redacted to the console.

## Installation

You can install the development version of ghosted like so:

``` r
# via remotes
remotes::install_github("abiraahmi/ghosted")

# or with pak
# pak::pak("abiraahmi/ghosted")
```

## Examples

``` r
library(ghosted)

# VTT → DOCX (or "vtt"/"txt")
ghost_vtt(
  filepath     = "inst/data/sample.vtt",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  out_format   = "docx",
  output_path  = "output/sample_DEID.docx",
  suffix       = "_DEID"
)

# DOCX → DOCX
ghost_docx(
  filepath     = "inst/data/transcript.docx",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_path  = "output/transcript_DEID.docx"
)

# TXT → TXT
ghost_txt(
  filepath     = "inst/data/notes.txt",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_path  = "output/notes_DEID.txt"
)

# Batch a folder (keep file types)
res <- ghost_batch(
  input_dir    = "inst/data/transcripts",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_dir   = "output",
  suffix       = "_DEID"
)
print(res)
```

Notes - Leading speaker tokens at line start (e.g., `Name:` or
`<v Name>`) are normalized to `Interviewer`/`Participant` while names
elsewhere are redacted. - For DOCX/TXT → VTT conversions in
`ghost_batch(out_format = "vtt")`, cues are written without
timestamps. - Ensure `officer` is installed for `.docx` reads/writes.
