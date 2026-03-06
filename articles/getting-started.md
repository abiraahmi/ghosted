# De-identifying Transcripts with ghosted

``` r
# Load the library
library(ghosted)
```

The ghosted package provides four high-level helpers to de‑identify
transcripts without forcing you to work with intermediate data.frames:

- [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md):
  redact a `.vtt` (Zoom/WebVTT) file and write `.vtt`/`.docx`/`.txt`.
- [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md):
  redact a `.docx` file and write `.docx`.
- [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md):
  redact a `.txt` file and write `.txt`.
- [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md):
  run the same logic across a folder of `.vtt`/`.docx`/`.txt` files.

Note: To ensure successful CRAN checks, examples avoid file I/O and
instead use small in‑memory objects.

## One‑file workflows (no data.frame needed)

``` r
# VTT → DOCX
ghost_vtt(
  filepath    = "path/to/meeting.vtt",
  interviewers = "Sansa Stark",
  interviewees = "Arya Stark",
  out_format   = "docx",            # or "vtt" / "txt"
  output_dir   = "output/",
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

- Progress is shown with a console progress bar.
- For DOCX/TXT → VTT conversions (when `out_format = "vtt"`), cues are
  written without timestamps.
- Install `officer` for DOCX reads/writes.
