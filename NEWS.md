# ghosted 0.0.1.8 (2026-03-04)

- New functions to allow for different file types: `ghost_vtt()`, `ghost_docx()`, `ghost_txt()`, and `ghost_batch()`.
- Leading speaker names at line start are normalized to `Interviewer`/`Participant`; names elsewhere are redacted.
- `ghost_batch()` adds progress bar and flexible `out_format` across input types.
- VTT conversion from DOCX/TXT writes cues without timestamps.
- README and vignette updated to reflect changes.

# ghosted 0.0.1.7 (2026-03-03)

- Added website with getting-started vignette.

# ghosted 0.0.1.6 (2026-03-01)

- Initial development version.
