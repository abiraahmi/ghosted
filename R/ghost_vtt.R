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
#' For TXT and DOCX outputs, each cue timestamp is written on the line above
#' the corresponding speaker/text line.
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
#' @param review_names If `TRUE`, open a local Shiny app to classify likely
#'   names detected by rule-based matching as interviewer, participant, or
#'   other redaction terms. The app also accepts manually typed `redact_other`
#'   terms. Defaults to `interactive()`.
#' @param name_review_min_score Minimum rule-based score for a candidate to
#'   appear in the review app.
#' @param show_completion_notice If `TRUE`, open a local completion notice after
#'   the redacted output is written. Defaults to `review_names`.
#' @return Invisibly, the output path written.
#' @examples
#' # Write redacted VTT next to source:
#' # ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # Write redacted DOCX with report:
#' # ghost_vtt("meeting.vtt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   out_format = "docx", report_redacted = TRUE)
#' @export
ghost_vtt <- function(filepath,
                      interviewers = character(),
                      interviewees = character(),
                      redact_other = character(),
                      redact_interviewer = TRUE,
                      include_common_names = FALSE,
                      redacted_token = "[REDACTED]",
                      add_blank_line_between_turns = TRUE,
                      output_path = NULL,
                      suffix = "_redacted",
                      out_format = c("vtt", "docx", "txt"),
                      report_redacted = FALSE,
                      review_names = interactive(),
                      name_review_min_score = 2,
                      show_completion_notice = review_names) {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .vtt file")
  }
  if (!file.exists(filepath)) stop("File not found: ", filepath)
  in_ext <- tolower(tools::file_ext(filepath))
  if (!identical(in_ext, "vtt")) stop("filepath must be a .vtt")

  fmt <- match.arg(out_format)

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
  review_text <- unlist(lapply(cues, `[[`, "text"), use.names = FALSE)
  reviewed <- review_redaction_terms(review_text, interviewers, interviewees,
                                     redact_other, review_names,
                                     name_review_min_score)
  interviewers <- reviewed$interviewers
  interviewees <- reviewed$interviewees
  redact_other <- reviewed$redact_other

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
  post_interviewer_optimizations <- 0L
  post_participant_optimizations <- 0L
  original_interviewer_names <- count_phrase_occurrences(all_text, interviewers)
  original_participant_names <- count_phrase_occurrences(all_text, interviewees)

  if (length(cues)) {
    for (k in seq_along(cues)) {
      tvec <- cues[[k]]$text
      tvec <- leading_speaker_label(tvec, sets$int_set, "Interviewer",
                                    vtt_voice_tag = TRUE)
      tvec <- leading_speaker_label(tvec, sets$ive_set, "Participant",
                                    vtt_voice_tag = TRUE)
      cues[[k]]$text <- tvec
    }
    cues <- collapse_consecutive_speaker_cues(cues)
    post_interviewer_optimizations <- attr(cues,
                                           "interviewer_optimizations",
                                           exact = TRUE)
    if (is.null(post_interviewer_optimizations)) {
      post_interviewer_optimizations <- 0L
    }
    post_participant_optimizations <- attr(cues,
                                           "participant_optimizations",
                                           exact = TRUE)
    if (is.null(post_participant_optimizations)) {
      post_participant_optimizations <- 0L
    }
  }
  formatted_text <- unlist(lapply(cues, `[[`, "text"), use.names = FALSE)
  post_interviewer_index <- count_speaker_index_labels(formatted_text,
                                                       "Interviewer")
  post_participant_index <- count_speaker_index_labels(formatted_text,
                                                       "Participant")
  if (isTRUE(report_redacted) && length(formatted_text)) {
    found_names <- phrases_found(formatted_text, sets$names_text)
    found_other <- phrases_found(formatted_text, sets$other_all)
  }
  phrase_groups <- build_redaction_phrase_groups(interviewers,
                                                 interviewees,
                                                 redact_interviewer,
                                                 sets$other_all)
  redaction_counts <- stats::setNames(integer(length(phrase_groups)),
                                      names(phrase_groups))
  if (length(cues)) {
    for (k in seq_along(cues)) {
      redacted_result <- redact_phrase_groups(cues[[k]]$text,
                                              phrase_groups,
                                              redacted_token)
      cues[[k]]$text <- redacted_result$text
      redaction_counts <- redaction_counts + redacted_result$counts
    }
  }
  redaction_report <- build_redaction_report(
    redaction_counts,
    original_interviewer_names,
    original_participant_names,
    post_interviewer_index,
    post_participant_index,
    post_interviewer_optimizations,
    post_participant_optimizations
  )

  if (isTRUE(report_redacted)) {
    report_redaction_summary(found_names, found_other)
    print_redaction_report(redaction_report)
  }

  output_path <- resolve_output_path(filepath, output_path, suffix, fmt)
  write_redacted_cues(cues, output_path, fmt, add_blank_line_between_turns)

  attr(output_path, "redaction_report") <- redaction_report
  show_redaction_complete(show_completion_notice, report = redaction_report)
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
    doc <- suppress_docx_namespace_warning(officer::read_docx())
    if (length(cues)) {
      out_lines <- format_cues_with_timestamps(cues,
                                               add_blank_line_between_turns)
      for (para in out_lines) {
        doc <- officer::body_add_par(doc, para, style = "Normal")
      }
    }
    suppress_docx_namespace_warning(print(doc, target = output_path))
  } else if (identical(fmt, "txt")) {
    out_lines <- format_cues_with_timestamps(cues,
                                             add_blank_line_between_turns)
    con <- file(output_path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(out_lines, con, sep = "\n", useBytes = TRUE)
  } else {
    stop("Unsupported out_format: ", fmt)
  }
}

#' Format VTT cues for TXT/DOCX while keeping timestamps above speaker labels
#' @noRd
format_cues_with_timestamps <- function(cues, add_blank_line_between_turns) {
  out_lines <- character()
  previous_speaker <- NA_character_

  if (!length(cues)) return(out_lines)

  for (cue in cues) {
    text_line <- if (length(cue$text)) paste(cue$text, collapse = " ") else ""
    current_speaker <- turn_speaker_label(text_line)

    if (isTRUE(add_blank_line_between_turns) &&
        length(out_lines) &&
        !is.na(previous_speaker) &&
        !is.na(current_speaker) &&
        !identical(previous_speaker, current_speaker)) {
      out_lines <- c(out_lines, "")
    }

    out_lines <- c(out_lines, cue$time, text_line)

    if (!is.na(current_speaker)) {
      previous_speaker <- current_speaker
    }
  }

  out_lines
}
