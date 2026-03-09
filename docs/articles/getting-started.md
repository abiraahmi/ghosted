# De-identifying Transcripts with ghosted

``` r
# Load the library
library(ghosted)
```

The ghosted package provides four high-level helpers to de‑identify
transcripts without forcing you to work with intermediate data.frames:

- [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md)
  — redact a `.vtt` (Zoom/WebVTT) file and write `.vtt`/`.docx`/`.txt`;
  supports `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_path`,
  `suffix`, `out_format`, `report_redacted`.
- [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md)
  — redact a `.docx` and write `.docx`/`.txt`/`.vtt`; supports
  `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_path`,
  `suffix`, `out_format`, `report_redacted`.
- [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md)
  — redact a `.txt` and write `.txt`/`.docx`/`.vtt`; supports
  `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_path`,
  `suffix`, `out_format`, `report_redacted`.
- [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md)
  — run the same logic across a folder of `.vtt`/`.docx`/`.txt` files;
  supports `redact_other`, `redact_interviewer`, `include_common_names`,
  `redacted_token`, `add_blank_line_between_turns`, `output_dir`,
  `suffix`, `out_format`, `report_redacted`.

Common arguments (ghost_vtt/ghost_docx/ghost_txt):

- `filepath`: input file path (`.vtt`/`.docx`/`.txt` respectively).
- `interviewers`: character vector of interviewer names (required).
- `interviewees`: character vector of participant names (optional).
- `redact_other`: additional words/phrases to redact.
- `redact_interviewer`: if `TRUE`, also redact interviewer names in
  text.
- `include_common_names`: if `TRUE`, include
  `ghosted::common_names_default()` if available.
- `redacted_token`: replacement token for redactions (default
  `[REDACTED]`).
- `add_blank_line_between_turns`: for DOCX/TXT outputs, insert a blank
  line between turns.
- `output_path`: explicit output file path; if `NULL`, uses input dir
  with `suffix`.
- `suffix`: appended to base name when auto-generating outputs (default
  `"_redacted"`).
- `out_format`: output type. Allowed values per function:
  - [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md):
    `"vtt"` (default), `"docx"`, `"txt"`
  - [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md):
    `"docx"` (default), `"txt"`, `"vtt"`
  - [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md):
    `"txt"` (default), `"docx"`, `"vtt"`
- `report_redacted`: if `TRUE`, prints which phrases were found and
  redacted.

## One‑file workflows

``` r
# VTT → DOCX
ghost_vtt(
  filepath    = "path/to/meeting.vtt",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  out_format   = "docx",            # or "vtt" / "txt"
  output_path  = "output/",
  suffix       = "_DEID"
)

# DOCX → DOCX
ghost_docx(
  filepath     = "path/to/transcript.docx",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_path  = "output/transcript_DEID.docx"
)

# TXT → TXT
ghost_txt(
  filepath     = "path/to/notes.txt",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_path  = "output/notes_DEID.txt"
)
```

Speaker labels at the start of a line/paragraph are normalized to
`Interviewer` or `Participant` (e.g., `Name:` or `<v Name>` →
`Interviewer:`), while names elsewhere are replaced with `[REDACTED]`.

## Batch a folder

``` r
res <- ghost_batch(
  input_dir    = "path/to/transcripts",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  output_dir   = "output/",
  suffix       = "_DEID",      # default
  out_format   = NULL           # keep each file’s type; or "docx"/"txt"/"vtt"
)
print(res)
```

Notes

- [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md):
  Progress is shown with a console progress bar.
- For DOCX/TXT → VTT conversions (when `out_format = "vtt"`), cues are
  written without timestamps.
- Leading speaker tokens at line start (e.g., `Name:` or `<v Name>`) are
  normalized to `Interviewer`/`Participant` while names elsewhere are
  redacted.
- For DOCX/TXT → VTT conversions in `ghost_batch(out_format = "vtt")`,
  cues are written without timestamps. - Ensure `officer` is installed
  for `.docx` reads/writes.
- Install `officer` for DOCX reads/writes.
