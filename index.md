# ghosted

De-identify transcripts without boilerplate. Four helpers:

- [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md)
  — redact a `.vtt` (Zoom/WebVTT) file and write `.vtt`/`.docx`/`.txt`.
- [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md)
  — redact a `.docx` and write `.docx`.
- [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md)
  — redact a `.txt` and write `.txt`.
- [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md)
  — run the same logic across a folder of `.vtt`/`.docx`/`.txt` files.

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
  output_dir   = "output",
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
