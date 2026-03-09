# Changelog

## ghosted 0.0.2.0 (2026-03-04)

- New functions to allow for different file types:
  [`ghost_vtt()`](https://abiraahmi.github.io/ghosted/reference/ghost_vtt.md),
  [`ghost_docx()`](https://abiraahmi.github.io/ghosted/reference/ghost_docx.md),
  [`ghost_txt()`](https://abiraahmi.github.io/ghosted/reference/ghost_txt.md),
  and
  [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md).
- Leading speaker names at line start are normalized to
  `Interviewer`/`Participant`; names elsewhere are redacted.
- [`ghost_batch()`](https://abiraahmi.github.io/ghosted/reference/ghost_batch.md)
  adds progress bar and flexible `out_format` across input types.
- VTT conversion from DOCX/TXT writes cues without timestamps.
- README and vignette updated to reflect changes.

## ghosted 0.0.1.7 (2026-03-03)

- Added website with getting-started vignette.

## ghosted 0.0.1.6 (2026-03-01)

- Initial development version.
