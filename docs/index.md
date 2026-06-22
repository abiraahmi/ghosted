# ghosted

De-identify transcript batches with one local app:

- [`ghost()`](https://abiraahmi.github.io/ghosted/reference/ghost.md)
  opens a local Shiny app for `.vtt`, `.docx`, and `.txt` transcripts.
- The app asks for input/output directories, known interviewer names,
  known participant names, and other terms to redact.
- It scans the input directory, reviews likely names detected with local
  rule-based matching, and writes redacted transcripts to the output
  directory.
- It also includes options for output format, filename suffix, redaction
  token, blank lines between turns, console reports, common-name
  redaction, and the completion notice.

The app runs locally on your computer and does not use AI or remote
services.

## Installation

You can install the development version of ghosted like so:

``` r
# via remotes
remotes::install_github("abiraahmi/ghosted")

# or with pak
# pak::pak("abiraahmi/ghosted")
```

## Use

``` r
library(ghosted)

ghost()
```

In the app:

1.  Enter the input directory containing `.vtt`, `.docx`, or `.txt`
    transcripts.
2.  Enter an output directory, or leave it blank to write outputs next
    to the input files.
3.  Choose processing options, such as output format and redaction
    token.
4.  List known interviewer and participant names, plus any other terms
    to redact.
5.  Click **Scan directory**.
6.  Review likely names and mark them as interviewer, participant, or
    other.
7.  Click **Redact transcripts**.

When you run
[`ghost()`](https://abiraahmi.github.io/ghosted/reference/ghost.md)
again in the same R session, the app preloads the directories, options,
names, and likely-name selections from the previous run.

## Notes

- Leading speaker tokens at line start, such as `Name:` or `<v Name>`,
  are normalized to `Interviewer` or `Participant`.
- Names elsewhere in the transcript body are replaced with the selected
  redaction token.
- For DOCX/TXT to VTT output, cues are written without timestamps.
- Install `officer` for DOCX reads and writes.
