#' Read a text file and redact it (no data.frame)
#'
#' Reads a `.txt` file line-by-line, applies in-function redaction, and writes a
#' redacted file. No speaker/text data.frame is created; the file is treated as a
#' sequence of lines. You can choose the output format (TXT/DOCX/VTT) via
#' `out_format` similar to [ghost_vtt()] and [ghost_batch()].
#'
#' @param filepath Path to a `.txt` file.
#' @param interviewers Character vector of interviewer names.
#' @param interviewees Character vector of interviewee/participant names.
#' @param redact_other Other words/phrases to redact.
#' @param redact_interviewer If `TRUE` (default), redact interviewer names in
#'   body text. If `FALSE`, interviewer names are preserved in body text;
#'   leading speaker labels are still normalized to `Interviewer` regardless.
#' @param include_common_names If `TRUE`, also redact a default list of common
#'   names (e.g., top US baby names, if available via
#'   `ghosted::common_names_default`). Emits a warning when the dataset is not
#'   bundled in the installed version.
#' @param redacted_token Replacement token used for redactions (names and other
#'   phrases).
#' @param add_blank_line_between_turns Logical; for DOCX outputs, insert a blank
#'   line between turns.
#' @param output_path Path for the redacted file. If `NULL` (default), set to the
#'   same directory and base name as `filepath` with `_redacted` before the
#'   extension. The extension is chosen based on `out_format` (e.g.
#'   `notes.txt` -> `notes_redacted.txt` for `out_format = "txt"`, or
#'   `notes_redacted.docx` / `notes_redacted.vtt` otherwise).
#' @param suffix Suffix to append to the base filename (default: `"_redacted"`).
#'   Only used when `output_path` is `NULL`.
#' @param out_format One of `"txt"`, `"docx"`, or `"vtt"` controlling the output
#'   file type. Defaults to `"txt"`.
#' @param report_redacted If `TRUE`, print to the R console which phrases were
#'   found and redacted (names and other).
#' @return The path to the written output file (invisibly).
#' @examples
#' # Writes notes_redacted.txt in same folder, returns path:
#' # ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # Write as DOCX instead of TXT:
#' # ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   out_format = "docx")
#' # With common names and redaction report:
#' # ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   include_common_names = TRUE, report_redacted = TRUE)
#' @export
ghost_txt <- function(filepath,
                      interviewers,
                      interviewees = character(),
                      redact_other = character(),
                      redact_interviewer = TRUE,
                      include_common_names = FALSE,
                      redacted_token = "[REDACTED]",
                      add_blank_line_between_turns = TRUE,
                      output_path = NULL,
                      suffix = "_redacted",
                      out_format = c("txt", "docx", "vtt"),
                      report_redacted = FALSE) {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .txt file")
  }
  if (!file.exists(filepath)) stop("File not found: ", filepath)
  in_ext <- tolower(tools::file_ext(filepath))
  if (!identical(in_ext, "txt")) stop("filepath must be a .txt")

  fmt <- match.arg(out_format)

  if (isTRUE(redact_interviewer) && length(interviewers) < 1) {
    stop("Provide interviewer names when redact_interviewer = TRUE")
  }

  lines <- read_txt_lines(filepath)

  sets <- build_phrase_sets(
    interviewers        = interviewers,
    interviewees        = interviewees,
    redact_other        = redact_other,
    redact_interviewer  = redact_interviewer,
    include_common_names = include_common_names
  )

  found_names <- character()
  found_other <- character()
  if (isTRUE(report_redacted)) {
    found_names <- phrases_found(lines, sets$names_text)
    found_other <- phrases_found(lines, sets$other_all)
  }

  lines <- leading_speaker_label(lines, sets$int_set, "Interviewer")
  lines <- leading_speaker_label(lines, sets$ive_set, "Participant")
  redacted <- redact_phrases(lines, sets$names_text, redacted_token)
  redacted <- redact_phrases(redacted, sets$other_all, redacted_token)

  if (isTRUE(report_redacted)) {
    if (length(found_names)) {
      message("Names redacted: ", paste(found_names, collapse = ", "))
    }
    if (length(found_other)) {
      message("Other phrases redacted: ", paste(found_other, collapse = ", "))
    }
  }

  output_path <- resolve_output_path(filepath, output_path, suffix, fmt)
  write_redacted_lines(redacted, output_path, fmt,
                       add_blank_line_between_turns)

  invisible(output_path)
}

# ---- TXT-specific helpers ---------------------------------------------------

#' Read a `.txt` file as UTF-8 lines, stripping BOM and trailing carriage returns
#' @noRd
read_txt_lines <- function(filepath) {
  lines <- tryCatch(readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character())
  if (!length(lines)) return(character())
  bom <- intToUtf8(0xFEFF)
  lines[1] <- sub(paste0("^", bom), "", lines[1], useBytes = TRUE)
  sub("\r$", "", lines)
}

#' Write redacted lines to disk as TXT, DOCX, or VTT
#'
#' For TXT output, lines are written verbatim (no inserted blank lines, since
#' the input already preserves line structure). For DOCX, optionally inserts
#' an empty paragraph between lines when `add_blank_line_between_turns` is set.
#' For VTT, each line becomes a numbered cue without a timestamp.
#' @noRd
write_redacted_lines <- function(redacted, output_path, fmt,
                                 add_blank_line_between_turns) {
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (identical(fmt, "txt")) {
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(redacted, con = con, sep = "\n", useBytes = TRUE)
  } else if (identical(fmt, "docx")) {
    doc <- officer::read_docx()
    if (length(redacted)) {
      for (ln in redacted) {
        doc <- officer::body_add_par(doc,
                                     ifelse(is.na(ln), "", ln),
                                     style = "Normal")
        if (add_blank_line_between_turns) {
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    }
    print(doc, target = output_path)
  } else if (identical(fmt, "vtt")) {
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines("WEBVTT", con)
    writeLines("", con)
    for (k in seq_along(redacted)) {
      writeLines(c(as.character(k),
                   ifelse(is.na(redacted[k]), "", redacted[k]),
                   ""), con)
    }
  } else {
    stop("Unsupported out_format: ", fmt)
  }
}
