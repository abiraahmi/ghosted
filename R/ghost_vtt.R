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
#' @param output_dir Output folder. If `NULL`, uses the folder of `filepath`.
#'   The output filename is derived from the input base name with `suffix`
#'   and an extension based on `out_format`.
#' @param out_format One of `"vtt"`, `"docx"`, or `"txt"` controlling the
#'   output file extension.
#' @param suffix Suffix to append to the base filename (default: `"_redacted"`).
#' @param redact_other Other words/phrases to redact.
#' @param redact_interviewer If `TRUE`, also redact interviewer names in the
#'   transcript text. Interviewer names in the speaker field are always redacted.
#' @param include_common_names If `TRUE`, also redact top US baby names (uses
#'   `common_names_fun`).
#' @param common_names_fun Function used when `include_common_names = TRUE`
#'   (default: top 1000 US baby names from `babynames`).
#' @param report_redacted If `TRUE`, prints which phrases were found and redacted.
#' @param name_token Replacement token for names.
#' @param school_token Replacement token for schools/other phrases.
#' @param add_blank_line_between_turns Logical; for DOCX/TXT outputs, insert a
#'   blank line between turns.
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
                           output_dir = NULL,
                           out_format = c("vtt", "docx", "txt"),
                           suffix = "_redacted",
                           redact_other = character(),
                           redact_interviewer = FALSE,
                           include_common_names = FALSE,
                           common_names_fun = NULL,
                           report_redacted = FALSE,
                           name_token = "[REDACTED]",
                           school_token = "[REDACTED]",
                           add_blank_line_between_turns = TRUE) {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .vtt file")
  }
  if (!base::file.exists(filepath)) base::stop("File not found: ", filepath)
  in_ext <- base::tolower(tools::file_ext(filepath))
  if (!base::identical(in_ext, "vtt")) base::stop("filepath must be a .vtt")

  fmt <- match.arg(out_format)

  # 1) Read raw lines and parse into cue blocks (no data.frame)
  lines <- base::tryCatch(base::readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) base::character())
  if (!base::length(lines)) lines <- base::character()
  if (base::length(lines)) {
    bom <- base::intToUtf8(0xFEFF)
    lines[1] <- base::sub(base::paste0("^", bom), "", lines[1], useBytes = TRUE)
  }
  lines <- base::sub("\r$", "", lines)

  # Minimal VTT cue parser to group text lines; preserves original id/time lines
  ts_re <- "^([0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{1,3})[[:space:]]-->[[:space:]]([0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{1,3})"
  cues <- list()
  i <- 1; n <- base::length(lines)
  # Skip optional WEBVTT header
  if (i <= n && grepl("^\\s*WEBVTT\\b", lines[i], perl = TRUE)) i <- i + 1

  while (i <= n) {
    # skip blank lines
    while (i <= n && base::trimws(lines[i]) == "") i <- i + 1
    if (i > n) break

    id_line <- NA_character_
    if (base::grepl("^[0-9]+$", base::trimws(lines[i]))) {
      id_line <- base::trimws(lines[i])
      i <- i + 1
    }
    if (i > n) break
    if (!base::grepl(ts_re, lines[i])) {
      # Not a valid cue; skip to next line
      i <- i + 1
      next
    }
    time_line <- lines[i]
    i <- i + 1
    txt <- base::character()
    while (i <= n && base::trimws(lines[i]) != "") {
      txt <- base::c(txt, lines[i])
      i <- i + 1
    }
    cues[[base::length(cues) + 1]] <- base::list(id = id_line, time = time_line, text = txt)
  }

  # If no cues parsed, proceed with empty content

  # 2) Redact speakers and text (standalone logic)
  if (isTRUE(redact_interviewer) && length(interviewers) < 1) {
    stop("Provide interviewer names when redact_interviewer = TRUE")
  }

  expand_name_parts <- function(full_names,
                                min_chars = 3,
                                drop_tokens = c("mr","mrs","ms","miss","dr","prof","sir","madam",
                                                "jr","sr","ii","iii","iv",
                                                "van","von","de","del","da","di","la","le","st","saint"),
                                keep_hyphenated_parts = TRUE) {
    full_names <- full_names[!is.na(full_names) & nzchar(full_names)]
    full_names <- trimws(full_names)
    parts <- unlist(strsplit(full_names, "\\s+", perl = TRUE), use.names = FALSE)
    parts <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", parts, perl = TRUE)
    if (keep_hyphenated_parts) {
      hy_sub <- unlist(strsplit(parts, "-", fixed = TRUE), use.names = FALSE)
      hy_sub <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", hy_sub, perl = TRUE)
      parts <- c(parts, hy_sub)
    }
    parts <- parts[nzchar(parts)]
    parts_low <- tolower(parts)
    drop_low <- tolower(drop_tokens)
    unique(parts[nchar(parts) >= min_chars & !(parts_low %in% drop_low)])
  }
  escape_regex <- function(x) gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", x, perl = TRUE)
  boundary_wrap <- function(escaped_phrase) paste0("(^|[^[:alnum:]_])(", escaped_phrase, ")([^[:alnum:]_]|$)")
  redact_phrases <- function(text_vec, phrases, token) {
    if (!length(phrases)) return(text_vec)
    phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
    for (p in phrases) {
      pat <- boundary_wrap(escape_regex(p))
      text_vec <- gsub(pat, paste0("\\1", token, "\\3"), text_vec, perl = TRUE, ignore.case = TRUE)
    }
    text_vec
  }
  phrases_found <- function(text_vec, phrases) {
    if (!length(phrases)) return(character())
    found <- character()
    for (p in phrases) {
      pat <- boundary_wrap(escape_regex(p))
      if (any(grepl(pat, text_vec, perl = TRUE, ignore.case = TRUE))) found <- c(found, p)
    }
    unique(found)
  }

  # Normalize inputs and build phrase sets
  interviewers <- interviewers[!base::is.na(interviewers) & base::nzchar(interviewers)]
  interviewees <- interviewees[!base::is.na(interviewees) & base::nzchar(interviewees)]
  redact_other <- redact_other[!base::is.na(redact_other) & base::nzchar(redact_other)]
  if (base::isTRUE(include_common_names)) {
    if (base::is.null(common_names_fun)) {
      common_names_fun <- base::try(base::get("common_names_default", envir = base::asNamespace("ghosted")), silent = TRUE)
    }
    if (base::is.function(common_names_fun)) {
      redact_other <- base::unique(base::c(redact_other, common_names_fun()))
    }
  }

  # Names used for TEXT redaction (redact both interviewer and interviewee names)
  names_text <- unique(c(interviewees, interviewers))
  parts_text <- expand_name_parts(names_text, min_chars = 3)
  names_all_text <- unique(c(names_text, parts_text))

  # Names used for SPEAKER redaction (always redact interviewer+interviewee names)
  names_speaker <- unique(c(interviewers, interviewees))
  parts_speaker <- expand_name_parts(names_speaker, min_chars = 3)
  names_all_speaker <- unique(c(names_speaker, parts_speaker))

  # Other phrases (expand into individual tokens as well)
  other_base <- base::unique(base::c(redact_other))
  other_parts <- expand_name_parts(other_base, min_chars = 3, drop_tokens = character())
  other_all <- unique(c(other_base, other_parts))

  # Report and apply redaction over cue text only
  all_text <- base::unlist(base::lapply(cues, `[[`, "text"), use.names = FALSE)
  if (base::isTRUE(report_redacted) && base::length(all_text)) {
    found_names <- phrases_found(all_text, names_all_text)
    found_other <- phrases_found(all_text, other_all)
  }
  # Leading speaker labels (e.g., <v Name>, <v [Name]>, [Name], <Name>, or Name:)
  # Preserve <v ...> tag but normalize its speaker to Interviewer/Participant.
  leading_speaker_label <- function(text_vec, names, label) {
    if (!length(names)) return(text_vec)
    ord <- order(nchar(names), decreasing = TRUE)
    names <- names[ord]
    for (nm in names) {
      nm_esc <- escape_regex(nm)
      # <v(classes) Name>
      pat_v <- paste0("(?i)^\\s*<\\s*v(\\.[^>]*)?\\s+", nm_esc, ">")
      rep_v <- paste0("<v\\1 ", label, ">")
      text_vec <- sub(pat_v, rep_v, text_vec, perl = TRUE)
      # <v(classes) [Name]>
      pat_vb <- paste0("(?i)^\\s*<\\s*v(\\.[^>]*)?\\s*\\[\\s*", nm_esc, "\\s*\\]>")
      rep_vb <- paste0("<v\\1 ", label, ">")
      text_vec <- sub(pat_vb, rep_vb, text_vec, perl = TRUE)
      # [Name] at start
      pat_br <- paste0("(?i)^\\s*\\[\\s*", nm_esc, "\\s*\\](\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_br, paste0(label, "\\1"), text_vec, perl = TRUE)
      # <Name> at start
      pat_ang <- paste0("(?i)^\\s*<\\s*", nm_esc, "\\s*>\\s*(\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_ang, paste0(label, "\\1"), text_vec, perl = TRUE)
      # Bare Name at start
      pat_bare <- paste0("(?i)^\\s*", nm_esc, "(\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_bare, paste0(label, "\\1"), text_vec, perl = TRUE)
    }
    text_vec
  }
  # Redact each cue's text lines
  if (length(cues)) {
    for (k in seq_along(cues)) {
      tvec <- cues[[k]]$text
      # Build candidate sets including parts so <v First> matches full names
      int_set <- unique(c(interviewers, expand_name_parts(interviewers, min_chars = 3)))
      ive_set <- unique(c(interviewees, expand_name_parts(interviewees, min_chars = 3)))
      # Label leading names (preserve <v ...> tags)
      tvec <- leading_speaker_label(tvec, int_set, "Interviewer")
      tvec <- leading_speaker_label(tvec, ive_set, "Participant")
      tvec <- redact_phrases(tvec, names_all_text, name_token)
      tvec <- redact_phrases(tvec, other_all, school_token)
      cues[[k]]$text <- tvec
    }
  }
  if (isTRUE(report_redacted)) {
    if (exists("found_names") && length(found_names)) message("Names redacted: ", paste(found_names, collapse = ", "))
    if (exists("found_other") && length(found_other)) message("Other phrases redacted: ", paste(found_other, collapse = ", "))
  }

  # 3) Determine output path
  # Build output path from folder + format
  base <- tools::file_path_sans_ext(basename(filepath))
  ext <- switch(fmt, vtt = ".vtt", docx = ".docx", txt = ".txt")
  if (is.null(output_dir)) output_dir <- dirname(filepath)
  if (!is.character(suffix) || length(suffix) != 1) suffix <- ""
  output_path <- file.path(output_dir, paste0(base, suffix, ext))
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # 4) Write in requested format
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
        para <- if (length(cues[[idx]]$text)) paste(cues[[idx]]$text, collapse = " ") else ""
        doc <- officer::body_add_par(doc, para, style = "Normal")
        if (add_blank_line_between_turns) doc <- officer::body_add_par(doc, "", style = "Normal")
      }
    }
    print(doc, target = output_path)
  } else if (identical(fmt, "txt")) {
    out_lines <- character()
    if (length(cues)) {
      for (idx in seq_along(cues)) {
        para <- if (length(cues[[idx]]$text)) paste(cues[[idx]]$text, collapse = " ") else ""
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

  invisible(output_path)
}
