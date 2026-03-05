#' Read a text file and redact it (no data.frame)
#'
#' Reads a `.txt` file line-by-line, applies in-function redaction, and writes a
#' redacted text file. No speaker/text data.frame is created; the file is treated
#' as a sequence of lines.
#'
#' @param filepath Path to a `.txt` file.
#' @param interviewers Character vector of interviewer names.
#' @param interviewees Character vector of interviewee/participant names.
#' @param output_path Path for the redacted `.txt`. If `NULL` (default), set to
#'   the same directory and base name as `filepath` with `_redacted` before the
#'   extension (e.g. `notes.txt` -> `notes_redacted.txt`).
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
#' @return The path to the written `.txt` file (invisibly).
#' @examples
#' # Writes notes_redacted.txt in same folder, returns path:
#' # ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe")
#' # With common names and redaction report:
#' # ghost_txt("notes.txt", interviewers = "Dr. Smith", interviewees = "Jane Doe",
#' #   include_common_names = TRUE, report_redacted = TRUE)
#' @export
ghost_txt <- function(filepath,
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
    stop("Provide a single 'filepath' to a .txt file")
  }
  if (!base::file.exists(filepath)) base::stop("File not found: ", filepath)
  ext <- base::tolower(tools::file_ext(filepath))
  if (!base::identical(ext, "txt")) base::stop("filepath must be a .txt")

  # 1) Read raw lines from the file
  lines <- base::tryCatch(base::readLines(filepath, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) base::character())
  if (!base::length(lines)) lines <- base::character()
  # Strip BOM from first line and trailing CRs
  if (length(lines)) {
    bom <- base::intToUtf8(0xFEFF)
    lines[1] <- base::sub(base::paste0("^", bom), "", lines[1], useBytes = TRUE)
  }
  lines <- base::sub("\r$", "", lines)

  # 2) Redact text (standalone logic; no shared dependency)
  if (base::isTRUE(redact_interviewer) && base::length(interviewers) < 1) {
    base::stop("Provide interviewer names when redact_interviewer = TRUE")
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

  # Redact both interviewer and interviewee names in text
  names_text <- unique(c(interviewees, interviewers))
  parts_text <- expand_name_parts(names_text, min_chars = 3)
  names_all_text <- unique(c(names_text, parts_text))

  other_base <- base::unique(base::c(redact_other))
  other_parts <- expand_name_parts(other_base, min_chars = 3, drop_tokens = character())
  other_all <- unique(c(other_base, other_parts))

  if (base::isTRUE(report_redacted)) {
    found_names <- phrases_found(lines, names_all_text)
    found_other <- phrases_found(lines, other_all)
  }

  # Replace leading speaker names with labels at start of line only
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
  lines <- leading_speaker_label(lines, interviewers, "Interviewer")
  lines <- leading_speaker_label(lines, interviewees, "Participant")

  redacted <- redact_phrases(lines, names_all_text, name_token)
  redacted <- redact_phrases(redacted, other_all, school_token)

  if (isTRUE(report_redacted)) {
    if (length(found_names)) message("Names redacted: ", paste(found_names, collapse = ", "))
    if (length(found_other)) message("Other phrases redacted: ", paste(found_other, collapse = ", "))
  }

  # 3) Determine output path
  if (is.null(output_path)) {
    base <- tools::file_path_sans_ext(basename(filepath))
    output_path <- file.path(dirname(filepath), paste0(base, "_redacted.txt"))
  }
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # 4) Write redacted lines to a new text file
  con <- base::file(output_path, open = "w", encoding = "UTF-8")
  on.exit(base::close(con), add = TRUE)
  base::writeLines(redacted, con = con, sep = "\n", useBytes = TRUE)

  base::invisible(output_path)
}
