#' Read a Word file and redact it (no data.frame)
#'
#' Reads a `.docx` file as raw paragraphs, applies in-function redaction, and writes a
#' redacted Word file. No speaker/text data.frame is created; the document is
#' treated as a sequence of paragraphs.
#'
#' @param filepath Path to a `.docx` file.
#' @param interviewers Character vector of interviewer names.
#' @param interviewees Character vector of interviewee/participant names.
#' @param output_path Path for the redacted `.docx`. If `NULL` (default), set to
#'   the same directory and base name as `filepath` with `_redacted` before the
#'   extension (e.g. `report.docx` -> `report_redacted.docx`).
#' @param redact_other Other words/phrases to redact.
#' @param redact_interviewer If `TRUE`, also redact interviewer names.
#' @param include_common_names If `TRUE`, also redact top US baby names (uses
#'   `common_names_fun`).
#' @param common_names_fun Function used when `include_common_names = TRUE`
#'   (default: top 1000 US baby names from `babynames`).
#' @param report_redacted If `TRUE`, print to the R console which phrases were
#'   found and redacted (names and other).
#' @param name_token Replacement token for names.
#' @param school_token Replacement token for schools/other phrases.
#' @return The path to the written `.docx` file (invisibly).
#' @examples
#' # Writes report_redacted.docx in same folder, returns path:
#' # ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # With common names and redaction report:
#' # ghost_docx("report.docx", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   include_common_names = TRUE, report_redacted = TRUE)
#' @export
ghost_docx <- function(filepath,
                        interviewers,
                        interviewees = character(),
                        output_path = NULL,
                        redact_other = character(),
                        redact_interviewer = FALSE,
                        include_common_names = FALSE,
                        common_names_fun = NULL,
                        report_redacted = FALSE,
                        name_token = "[REDACTED]",
                        school_token = "[REDACTED]") {

  if (!is.character(filepath) || length(filepath) != 1 || !nzchar(filepath)) {
    stop("Provide a single 'filepath' to a .docx file")
  }
  if (!base::file.exists(filepath)) base::stop("File not found: ", filepath)
  ext <- base::tolower(tools::file_ext(filepath))
  if (!base::identical(ext, "docx")) base::stop("filepath must be a .docx")

  # 1) Read raw paragraphs from the docx
  doc <- officer::read_docx(path = filepath)
  ds <- officer::docx_summary(doc)
  # Prefer paragraph-level grouping when available
  if ("content_type" %in% names(ds)) {
    ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
  }

  paragraphs <- character()
  if (nrow(ds)) {
    # Group by paragraph_id when present; otherwise treat each row as a paragraph
    if ("paragraph_id" %in% names(ds)) {
      split_par <- split(ds$text, ds$paragraph_id)
      paragraphs <- base::vapply(split_par, function(v) {
        v <- v[!base::is.na(v)]
        if (!base::length(v)) "" else base::paste(v, collapse = "")
      }, base::character(1))
      paragraphs <- unname(paragraphs)
    } else {
      paragraphs <- ds$text
      paragraphs[base::is.na(paragraphs)] <- ""
    }
  }

  # 2) Redact text (standalone logic; no redact_text dependency)
  if (base::isTRUE(redact_interviewer) && base::length(interviewers) < 1) {
    base::stop("Provide interviewer names when redact_interviewer = TRUE")
  }

  # Inline helpers
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

  interviewers <- interviewers[!is.na(interviewers) & nzchar(interviewers)]
  interviewees <- interviewees[!is.na(interviewees) & nzchar(interviewees)]
  redact_other <- redact_other[!is.na(redact_other) & nzchar(redact_other)]
  if (base::isTRUE(include_common_names)) {
    if (base::is.null(common_names_fun)) {
      common_names_fun <- base::try(base::get("common_names_default", envir = base::asNamespace("ghosted")), silent = TRUE)
    }
    if (base::is.function(common_names_fun)) {
      redact_other <- base::unique(base::c(redact_other, common_names_fun()))
    }
  }

  # Names (text): redact both interviewer and interviewee names in text
  names_text <- unique(c(interviewees, interviewers))
  parts_text <- expand_name_parts(names_text, min_chars = 3)
  names_all_text <- unique(c(names_text, parts_text))

  # Other phrases: also expand into parts without dropping tokens
  other_base <- unique(c(redact_other))
  other_parts <- expand_name_parts(other_base, min_chars = 3, drop_tokens = character())
  other_all <- unique(c(other_base, other_parts))

  if (isTRUE(report_redacted)) {
    found_names <- phrases_found(paragraphs, names_all_text)
    found_other <- phrases_found(paragraphs, other_all)
  }

  # Replace leading speaker names with labels at paragraph start only
  leading_speaker_label <- function(text_vec, names, label) {
    if (!length(names)) return(text_vec)
    ord <- order(nchar(names), decreasing = TRUE)
    names <- names[ord]
    for (nm in names) {
      nm_esc <- escape_regex(nm)
      # [Name] at start
      pat_br <- paste0("^\\s*\\[\\s*", nm_esc, "\\s*\\](\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_br, paste0(label, "\\1"), text_vec, perl = TRUE, ignore.case = TRUE)
      # <Name> at start
      pat_ang <- paste0("^\\s*<\\s*", nm_esc, "\\s*>\\s*(\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_ang, paste0(label, "\\1"), text_vec, perl = TRUE, ignore.case = TRUE)
      # Bare Name at start
      pat_bare <- paste0("^\\s*", nm_esc, "(\\s*[:\\-]?\\s*)")
      text_vec <- sub(pat_bare, paste0(label, "\\1"), text_vec, perl = TRUE, ignore.case = TRUE)
    }
    text_vec
  }
  paragraphs <- leading_speaker_label(paragraphs, interviewers, "Interviewer")
  paragraphs <- leading_speaker_label(paragraphs, interviewees, "Participant")

  redacted <- redact_phrases(paragraphs, names_all_text, name_token)
  redacted <- redact_phrases(redacted, other_all, school_token)

  if (isTRUE(report_redacted)) {
    if (length(found_names)) message("Names redacted: ", paste(found_names, collapse = ", "))
    if (length(found_other)) message("Other phrases redacted: ", paste(found_other, collapse = ", "))
  }

  # 3) Determine output path
  if (is.null(output_path)) {
  base <- tools::file_path_sans_ext(base::basename(filepath))
  output_path <- base::file.path(base::dirname(filepath), base::paste0(base, "_redacted.docx"))
  }
  out_dir <- base::dirname(output_path)
  if (!base::dir.exists(out_dir)) base::dir.create(out_dir, recursive = TRUE)

  # 4) Write the redacted paragraphs to a new docx
  out_doc <- officer::read_docx()
  if (base::length(redacted)) {
    for (p in redacted) {
      officer::body_add_par(out_doc, base::ifelse(base::is.na(p), "", p), style = "Normal")
    }
  } else {
    # Ensure the file still gets created even if there were no paragraphs
    officer::body_add_par(out_doc, "", style = "Normal")
  }

  print(out_doc, target = output_path)
  base::invisible(output_path)
}
