#' Redact a Zoom/WebVTT transcript and write VTT/DOCX/TXT
#'
#' Parses a Zoom/WebVTT transcript as raw cues (no data.frame), redacts
#' interviewee names (and optionally interviewer names) plus other phrases using
#' boundary-aware matching, and writes the result as a WebVTT, Word, or plain
#' text file.
#'
#' Redaction mirrors the standalone logic used in [ghost_docx()] and
#' [ghost_txt()]: full names are also split into parts (e.g., first/last and
#' hyphenated pieces) and replaced longest-first with tokens.
#'
#' @param filepath Path to a `.vtt` file.
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
#' @param add_blank_line_between_turns Logical; for DOCX/TXT outputs, insert a
#'   blank line between turns.
#' @param output_path Full path for the output file. If `NULL`, uses the folder
#'   of `filepath` with the input base name plus `suffix` and an extension based
#'   on `out_format`.
#' @param suffix Suffix to append to the base filename (default: `"_redacted"`).
#' @param out_format One of `"vtt"`, `"docx"`, or `"txt"` controlling the
#'   output file extension.
#' @param report_redacted If `TRUE`, prints which phrases were found and redacted.
#' @return Invisibly, the output path written.
#' @examples
#' # Write redacted VTT next to source:
#' # ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # Write redacted DOCX with report:
#' # ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   out_format = "docx", report_redacted = TRUE)
#' @export
ghost_vtt <- function(filepath,
                      interviewers,
                      interviewees = character(),
                      redact_other = character(),
                      redact_interviewer = TRUE,
                      include_common_names = FALSE,
                      redacted_token = "[REDACTED]",
                      add_blank_line_between_turns = TRUE,
                      output_path = NULL,
                      suffix = "_redacted",
                      out_format = c("vtt", "docx", "txt"),
                      report_redacted = FALSE) {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .vtt file")
  }
  if (!file.exists(filepath)) stop("File not found: ", filepath)
  in_ext <- tolower(tools::file_ext(filepath))
  if (!identical(in_ext, "vtt")) stop("filepath must be a .vtt")

  fmt <- match.arg(out_format)

  if (isTRUE(redact_interviewer) && length(interviewers) < 1) {
    stop("Provide interviewer names when redact_interviewer = TRUE")
  }

  # Read raw lines and parse into cue blocks (no data.frame)
  lines <- tryCatch(readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character())
  if (!length(lines)) lines <- character()
  if (length(lines)) {
    bom <- intToUtf8(0xFEFF)
    lines[1] <- sub(paste0("^", bom), "", lines[1], useBytes = TRUE)
  }
  lines <- sub("\r$", "", lines)

  cues <- parse_vtt_cues(lines)

  sets <- build_phrase_sets(
    interviewers        = interviewers,
    interviewees        = interviewees,
    redact_other        = redact_other,
    redact_interviewer  = redact_interviewer,
    include_common_names = include_common_names
  )

  all_text <- unlist(lapply(cues, `[[`, "text"), use.names = FALSE)
  found_names <- character()
  found_other <- character()
  if (isTRUE(report_redacted) && length(all_text)) {
    found_names <- phrases_found(all_text, sets$names_text)
    found_other <- phrases_found(all_text, sets$other_all)
  }

  if (length(cues)) {
    for (k in seq_along(cues)) {
      tvec <- cues[[k]]$text
      tvec <- leading_speaker_label(tvec, sets$int_set, "Interviewer",
                                    vtt_voice_tag = TRUE)
      tvec <- leading_speaker_label(tvec, sets$ive_set, "Participant",
                                    vtt_voice_tag = TRUE)
      tvec <- redact_phrases(tvec, sets$names_text, redacted_token)
      tvec <- redact_phrases(tvec, sets$other_all, redacted_token)
      cues[[k]]$text <- tvec
    }
  }

  if (isTRUE(report_redacted)) {
    if (length(found_names)) {
      message("Names redacted: ", paste(found_names, collapse = ", "))
    }
    if (length(found_other)) {
      message("Other phrases redacted: ", paste(found_other, collapse = ", "))
    }
  }

  output_path <- resolve_output_path(filepath, output_path, suffix, fmt)
  write_redacted_cues(cues, output_path, fmt, add_blank_line_between_turns)

  invisible(output_path)
}

# ---- VTT-specific helpers (kept here because they're not shared) ------------

#' Parse VTT lines into a list of cue records
#'
#' Each cue is a list with `id` (character or `NA`), `time` (the timestamp
#' line), and `text` (character vector of body lines). Skips the optional
#' `WEBVTT` header and any non-cue lines.
#' @noRd
parse_vtt_cues <- function(lines) {
  ts_re <- "^([0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{1,3})[[:space:]]-->[[:space:]]([0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{1,3})"
  cues <- list()
  i <- 1
  n <- length(lines)
  if (i <= n && grepl("^\\s*WEBVTT\\b", lines[i], perl = TRUE)) i <- i + 1

  while (i <= n) {
    while (i <= n && trimws(lines[i]) == "") i <- i + 1
    if (i > n) break

    id_line <- NA_character_
    if (grepl("^[0-9]+$", trimws(lines[i]))) {
      id_line <- trimws(lines[i])
      i <- i + 1
    }
    if (i > n) break
    if (!grepl(ts_re, lines[i])) {
      i <- i + 1
      next
    }
    time_line <- lines[i]
    i <- i + 1
    txt <- character()
    while (i <= n && trimws(lines[i]) != "") {
      txt <- c(txt, lines[i])
      i <- i + 1
    }
    cues[[length(cues) + 1]] <- list(id = id_line, time = time_line, text = txt)
  }
  cues
}

#' Write redacted cues to disk as VTT, DOCX, or TXT
#' @noRd
write_redacted_cues <- function(cues, output_path, fmt,
                                add_blank_line_between_turns) {
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  if (identical(fmt, "vtt")) {
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines("WEBVTT", con)
    writeLines("", con)
    if (length(cues)) {
      for (idx in seq_along(cues)) {
        id_line <- if (!is.na(cues[[idx]]$id)) cues[[idx]]$id else as.character(idx)
        writeLines(id_line, con)
        writeLines(cues[[idx]]$time, con)
        if (length(cues[[idx]]$text)) writeLines(cues[[idx]]$text, con)
        writeLines("", con)
      }
    }
  } else if (identical(fmt, "docx")) {
    doc <- officer::read_docx()
    if (length(cues)) {
      for (idx in seq_along(cues)) {
        para <- if (length(cues[[idx]]$text)) {
          paste(cues[[idx]]$text, collapse = " ")
        } else ""
        doc <- officer::body_add_par(doc, para, style = "Normal")
        if (add_blank_line_between_turns) {
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    }
    print(doc, target = output_path)
  } else if (identical(fmt, "txt")) {
    out_lines <- character()
    if (length(cues)) {
      for (idx in seq_along(cues)) {
        para <- if (length(cues[[idx]]$text)) {
          paste(cues[[idx]]$text, collapse = " ")
        } else ""
        out_lines <- c(out_lines, para)
        if (add_blank_line_between_turns) out_lines <- c(out_lines, "")
      }
    }
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(out_lines, con, sep = "\n", useBytes = TRUE)
  } else {
    stop("Unsupported out_format: ", fmt)
  }
}
