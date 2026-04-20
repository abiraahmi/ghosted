#' Read a Word file and redact it (no data.frame)
#'
#' Reads a `.docx` file as raw paragraphs, applies in-function redaction, and writes a
#' redacted file. No speaker/text data.frame is created; the document is treated
#' as a sequence of paragraphs. You can choose the output format (DOCX/TXT/VTT)
#' via `out_format` similar to [ghost_vtt()] and [ghost_batch()].
#'
#' @param filepath Path to a `.docx` file.
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
#' @param add_blank_line_between_turns Logical; for TXT/DOCX outputs when
#'   converting formats, insert a blank line between turns. This does not affect
#'   DOCX→DOCX.
#' @param output_path Path for the redacted file. If `NULL` (default), set to the
#'   same directory and base name as `filepath` with `_redacted` before the
#'   extension. The extension is chosen based on `out_format` (e.g.
#'   `report.docx` -> `report_redacted.docx` for `out_format = "docx"`, or
#'   `report_redacted.txt` / `report_redacted.vtt` otherwise).
#' @param suffix Suffix to append to the base filename (default: `"_redacted"`).
#'   Only used when `output_path` is `NULL`.
#' @param out_format One of `"docx"`, `"txt"`, or `"vtt"` controlling the output
#'   file type. Defaults to `"docx"`.
#' @param report_redacted If `TRUE`, print to the R console which phrases were
#'   found and redacted (names and other).
#' @return The path to the written output file (invisibly).
#' @examples
#' # Writes report_redacted.docx in same folder, returns path:
#' # ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # Write as TXT instead of DOCX:
#' # ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   out_format = "txt")
#' # With common names and redaction report:
#' # ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   include_common_names = TRUE, report_redacted = TRUE)
#' @export
ghost_docx <- function(filepath,
                       interviewers,
                       interviewees = character(),
                       redact_other = character(),
                       redact_interviewer = TRUE,
                       include_common_names = FALSE,
                       redacted_token = "[REDACTED]",
                       add_blank_line_between_turns = TRUE,
                       output_path = NULL,
                       suffix = "_redacted",
                       out_format = c("docx", "txt", "vtt"),
                       report_redacted = FALSE) {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .docx file")
  }
  if (!file.exists(filepath)) stop("File not found: ", filepath)
  in_ext <- tolower(tools::file_ext(filepath))
  if (!identical(in_ext, "docx")) stop("filepath must be a .docx")

  fmt <- match.arg(out_format)

  if (isTRUE(redact_interviewer) && length(interviewers) < 1) {
    stop("Provide interviewer names when redact_interviewer = TRUE")
  }

  paragraphs <- read_docx_paragraphs(filepath)

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
    found_names <- phrases_found(paragraphs, sets$names_text)
    found_other <- phrases_found(paragraphs, sets$other_all)
  }

  paragraphs <- leading_speaker_label(paragraphs, sets$int_set, "Interviewer")
  paragraphs <- leading_speaker_label(paragraphs, sets$ive_set, "Participant")
  redacted <- redact_phrases(paragraphs, sets$names_text, redacted_token)
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
  write_redacted_paragraphs(redacted, output_path, fmt,
                            add_blank_line_between_turns)

  invisible(output_path)
}

# ---- DOCX-specific helpers --------------------------------------------------

#' Read a `.docx` as a character vector of paragraph strings
#'
#' Uses `officer::docx_summary()` to extract paragraph text, grouping by
#' `paragraph_id` when available so multi-run paragraphs are concatenated.
#' Returns `character(0)` for an empty document.
#' @noRd
read_docx_paragraphs <- function(filepath) {
  doc <- officer::read_docx(path = filepath)
  ds <- officer::docx_summary(doc)
  # docx_summary() returns NULL for an empty document, not a 0-row data.frame.
  if (is.null(ds) || !is.data.frame(ds) || !nrow(ds)) return(character())
  if ("content_type" %in% names(ds)) {
    ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
  }
  if (!nrow(ds)) return(character())
  if ("paragraph_id" %in% names(ds)) {
    sp <- split(ds$text, ds$paragraph_id)
    out <- vapply(sp, function(v) {
      v <- v[!is.na(v)]
      if (!length(v)) "" else paste(v, collapse = "")
    }, character(1))
    return(unname(out))
  }
  out <- ds$text
  out[is.na(out)] <- ""
  out
}

#' Write redacted paragraphs to disk as DOCX, TXT, or VTT
#' @noRd
write_redacted_paragraphs <- function(redacted, output_path, fmt,
                                      add_blank_line_between_turns) {
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (identical(fmt, "docx")) {
    out_doc <- officer::read_docx()
    if (length(redacted)) {
      for (p in redacted) {
        out_doc <- officer::body_add_par(out_doc,
                                         ifelse(is.na(p), "", p),
                                         style = "Normal")
      }
    } else {
      out_doc <- officer::body_add_par(out_doc, "", style = "Normal")
    }
    print(out_doc, target = output_path)
  } else if (identical(fmt, "txt")) {
    out_lines <- redacted
    if (isTRUE(add_blank_line_between_turns) && length(out_lines)) {
      out_lines <- as.vector(rbind(out_lines, ""))
    }
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(out_lines, con = con, sep = "\n", useBytes = TRUE)
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
