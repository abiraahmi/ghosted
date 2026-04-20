# Internal helpers shared by ghost_vtt(), ghost_docx(), and ghost_txt().
# Not exported; document with @noRd to keep them out of the package index.

#' Expand full names into their component parts
#'
#' Splits each full name on whitespace (and optionally hyphens), strips
#' surrounding punctuation, drops common honorifics/particles, and returns the
#' unique set of parts at least `min_chars` characters long.
#'
#' @param full_names Character vector of full names.
#' @param min_chars Minimum character length for a part to be retained.
#' @param drop_tokens Lowercased tokens to discard (e.g., "dr", "jr", "van").
#' @param keep_hyphenated_parts If `TRUE`, also include sub-parts split on `-`.
#' @return Character vector of unique name parts.
#' @noRd
expand_name_parts <- function(full_names,
                              min_chars = 3,
                              drop_tokens = c("mr", "mrs", "ms", "miss", "dr", "prof", "sir", "madam",
                                              "jr", "sr", "ii", "iii", "iv",
                                              "van", "von", "de", "del", "da", "di", "la", "le", "st", "saint"),
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

#' Escape regex metacharacters in a literal string
#' @noRd
escape_regex <- function(x) {
  gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", x, perl = TRUE)
}

#' Wrap an already-escaped phrase with capturing word boundaries
#'
#' Returns a Perl regex that matches the phrase only when surrounded by
#' non-word characters or the start/end of the string. The wrap captures the
#' boundary characters so they can be preserved via backreferences `\\1` and
#' `\\3` during substitution.
#' @noRd
boundary_wrap <- function(escaped_phrase) {
  paste0("(^|[^[:alnum:]_])(", escaped_phrase, ")([^[:alnum:]_]|$)")
}

#' Replace each phrase with `token`, longest-first to avoid partial overlaps
#'
#' @param text_vec Character vector of text lines.
#' @param phrases Character vector of literal phrases to redact.
#' @param token Replacement token (e.g., `"[REDACTED]"`).
#' @return `text_vec` with all matches of any phrase replaced.
#' @noRd
redact_phrases <- function(text_vec, phrases, token) {
  if (!length(phrases)) return(text_vec)
  phrases <- phrases[order(nchar(phrases), decreasing = TRUE)]
  for (p in phrases) {
    pat <- boundary_wrap(escape_regex(p))
    text_vec <- gsub(pat, paste0("\\1", token, "\\3"), text_vec,
                     perl = TRUE, ignore.case = TRUE)
  }
  text_vec
}

#' Return the subset of `phrases` that appear at least once in `text_vec`
#' @noRd
phrases_found <- function(text_vec, phrases) {
  if (!length(phrases)) return(character())
  found <- character()
  for (p in phrases) {
    pat <- boundary_wrap(escape_regex(p))
    if (any(grepl(pat, text_vec, perl = TRUE, ignore.case = TRUE))) {
      found <- c(found, p)
    }
  }
  unique(found)
}

#' Replace a leading speaker name in each line with a role label
#'
#' Handles `[Name]`, `<Name>`, and bare `Name` at the start of a line, with an
#' optional trailing `:` or `-`. When `vtt_voice_tag = TRUE`, also handles VTT
#' `<v Name>` and `<v [Name]>` voice tags, preserving any class suffix.
#'
#' @param text_vec Character vector of text lines.
#' @param names Character vector of names (and name parts) to match.
#' @param label Replacement label (e.g., `"Interviewer"` or `"Participant"`).
#' @param vtt_voice_tag If `TRUE`, also rewrite `<v ...>` tags.
#' @noRd
leading_speaker_label <- function(text_vec, names, label, vtt_voice_tag = FALSE) {
  if (!length(names)) return(text_vec)
  names <- names[order(nchar(names), decreasing = TRUE)]
  for (nm in names) {
    nm_esc <- escape_regex(nm)
    if (vtt_voice_tag) {
      pat_v <- paste0("(?i)^\\s*<\\s*v(\\.[^>]*)?\\s+", nm_esc, ">")
      text_vec <- sub(pat_v, paste0("<v\\1 ", label, ">"), text_vec, perl = TRUE)
      pat_vb <- paste0("(?i)^\\s*<\\s*v(\\.[^>]*)?\\s*\\[\\s*", nm_esc, "\\s*\\]>")
      text_vec <- sub(pat_vb, paste0("<v\\1 ", label, ">"), text_vec, perl = TRUE)
    }
    pat_br <- paste0("^\\s*\\[\\s*", nm_esc, "\\s*\\](\\s*[:\\-]?\\s*)")
    text_vec <- sub(pat_br, paste0(label, "\\1"), text_vec,
                    perl = TRUE, ignore.case = TRUE)
    pat_ang <- paste0("^\\s*<\\s*", nm_esc, "\\s*>\\s*(\\s*[:\\-]?\\s*)")
    text_vec <- sub(pat_ang, paste0(label, "\\1"), text_vec,
                    perl = TRUE, ignore.case = TRUE)
    pat_bare <- paste0("^\\s*", nm_esc, "(\\s*[:\\-]?\\s*)")
    text_vec <- sub(pat_bare, paste0(label, "\\1"), text_vec,
                    perl = TRUE, ignore.case = TRUE)
  }
  text_vec
}

#' Resolve the optional bundled common-names dataset
#'
#' Looks up `common_names_default` in the package namespace. If it exists and
#' is callable, returns its value; otherwise, emits a one-time warning and
#' returns an empty character vector. This keeps `include_common_names = TRUE`
#' from silently doing nothing when the dataset isn't bundled.
#' @noRd
resolve_common_names <- function() {
  cnf <- try(get("common_names_default", envir = asNamespace("ghosted")),
             silent = TRUE)
  if (inherits(cnf, "try-error") || !is.function(cnf)) {
    warning(
      "include_common_names = TRUE was requested, but no `common_names_default` ",
      "dataset is bundled with this version of ghosted. Ignoring.",
      call. = FALSE
    )
    return(character())
  }
  out <- try(cnf(), silent = TRUE)
  if (inherits(out, "try-error") || !is.character(out)) {
    warning(
      "`common_names_default()` did not return a character vector. Ignoring.",
      call. = FALSE
    )
    return(character())
  }
  out
}

#' Build the redaction phrase sets used by all three ghost_*() entry points
#'
#' Centralizes the policy decisions (which names to redact in body text vs.
#' speaker labels, how to expand parts, and whether to honor
#' `redact_interviewer` and `include_common_names`).
#'
#' @param interviewers,interviewees Character vectors of full names.
#' @param redact_other Additional literal phrases to redact.
#' @param redact_interviewer If `TRUE` (default), interviewer names are also
#'   redacted in body text. If `FALSE`, they are preserved in the body but
#'   their leading speaker labels are still normalized.
#' @param include_common_names If `TRUE`, append the bundled common-names
#'   dataset to `redact_other` (warns if not available).
#' @return A list with `names_text`, `other_all`, `int_set`, and `ive_set`,
#'   where `int_set`/`ive_set` are full names plus expanded parts used for
#'   leading-speaker-label rewriting.
#' @noRd
build_phrase_sets <- function(interviewers,
                              interviewees,
                              redact_other,
                              redact_interviewer,
                              include_common_names) {
  interviewers <- interviewers[!is.na(interviewers) & nzchar(interviewers)]
  interviewees <- interviewees[!is.na(interviewees) & nzchar(interviewees)]
  redact_other <- redact_other[!is.na(redact_other) & nzchar(redact_other)]

  if (isTRUE(include_common_names)) {
    redact_other <- unique(c(redact_other, resolve_common_names()))
  }

  body_names <- if (isTRUE(redact_interviewer)) {
    unique(c(interviewees, interviewers))
  } else {
    interviewees
  }
  parts_text <- expand_name_parts(body_names, min_chars = 3)
  names_text <- unique(c(body_names, parts_text))

  other_parts <- expand_name_parts(redact_other, min_chars = 3,
                                   drop_tokens = character())
  other_all <- unique(c(redact_other, other_parts))

  int_set <- unique(c(interviewers,
                      expand_name_parts(interviewers, min_chars = 3)))
  ive_set <- unique(c(interviewees,
                      expand_name_parts(interviewees, min_chars = 3)))

  list(
    names_text = names_text,
    other_all  = other_all,
    int_set    = int_set,
    ive_set    = ive_set
  )
}

#' Compute the default output path for a redacted file
#'
#' If `output_path` is provided, returns it unchanged. Otherwise builds a path
#' next to `filepath` using its stem, the requested `suffix`, and an extension
#' derived from `fmt`. Creates the parent directory if needed.
#' @noRd
resolve_output_path <- function(filepath, output_path, suffix, fmt) {
  if (!is.null(output_path)) return(output_path)
  stem <- tools::file_path_sans_ext(basename(filepath))
  ext <- switch(fmt, vtt = ".vtt", docx = ".docx", txt = ".txt",
                stop("Unsupported out_format: ", fmt))
  if (!is.character(suffix) || length(suffix) != 1) suffix <- ""
  out <- file.path(dirname(filepath), paste0(stem, suffix, ext))
  out_dir <- dirname(out)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out
}
