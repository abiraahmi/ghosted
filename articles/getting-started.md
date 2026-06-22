# De-identifying Transcripts with ghosted

``` r

library(ghosted)
```

The ghosted package provides one user-facing function:
[`ghost()`](https://abiraahmi.github.io/ghosted/reference/ghost.md).

[`ghost()`](https://abiraahmi.github.io/ghosted/reference/ghost.md)
opens a local Shiny app for de-identifying batches of `.vtt`, `.docx`,
and `.txt` transcript files. The app asks for the information needed to
redact names and other terms, scans the selected input directory, lets
you review likely names detected with local rule-based matching, and
writes redacted transcripts to your selected output directory.

The app runs locally on your computer and does not use AI or remote
services.

Installing `ghosted` also installs the runtime packages the app needs,
including `shiny` for the app interface and `officer` for DOCX reads and
writes.

## Start The App

``` r

ghost()
```

## App Workflow

1.  Enter the input directory containing transcript files.
2.  Enter an output directory, or leave it blank to use the input
    directory.
3.  Choose processing options:
    - output format: keep original formats, DOCX, TXT, or VTT
    - output filename suffix
    - redaction token
    - whether to add blank lines between turns
    - whether to redact interviewer names in transcript body text
    - whether to include common-name redaction
    - whether to print a console report
    - whether to show the completion notice
4.  List known interviewer names, participant names, and any other terms
    to redact.
5.  Click **Scan directory**.
6.  Review likely names and classify them as interviewer, participant,
    or other.
7.  Click **Redact transcripts**.

## Re-running The App

When you run
[`ghost()`](https://abiraahmi.github.io/ghosted/reference/ghost.md)
again in the same R session, the app preloads the directories, options,
names, and likely-name selections from the previous run. If you are
working on a different batch of transcripts, update the relevant
sections before scanning and redacting.

## Output Behavior

- Speaker labels at the start of a line or paragraph are normalized to
  `Interviewer` or `Participant`.
- Names elsewhere in transcript body text are replaced with the selected
  redaction token.
- Other terms are redacted exactly as entered.
- For DOCX/TXT to VTT output, cues are written without timestamps.
