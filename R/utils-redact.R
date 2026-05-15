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

#' Suppress harmless DOCX namespace warnings from the XML stack
#'
#' Some valid Word files trigger `Undefined namespace prefix` warnings while
#' officer/xml2 inspect document properties. The redaction logic does not use
#' those properties, so this helper hides only that specific warning.
#' @noRd
suppress_docx_namespace_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("Undefined namespace prefix", conditionMessage(w),
                fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
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

#' Count non-overlapping matches for a Perl regex in a character vector
#' @noRd
count_regex_matches <- function(text_vec, pattern, ignore.case = TRUE) {
  text_vec <- text_vec[!is.na(text_vec)]
  if (!length(text_vec)) return(0L)

  matches <- gregexpr(pattern, text_vec, perl = TRUE,
                     ignore.case = ignore.case)
  sum(vapply(matches, function(m) {
    if (length(m) == 1 && m[1] == -1L) 0L else length(m)
  }, integer(1)))
}

#' Count literal phrase occurrences using the package's boundary rules
#' @noRd
count_phrase_occurrences <- function(text_vec, phrases) {
  if (!length(phrases)) return(0L)

  phrases <- phrases[!is.na(phrases) & nzchar(phrases)]
  if (!length(phrases)) return(0L)

  sum(vapply(phrases, function(p) {
    count_regex_matches(text_vec, boundary_wrap(escape_regex(p)))
  }, integer(1)))
}

#' Redact phrase groups and count replacements made by group
#'
#' Groups are redacted together longest-first, so full names win before parts.
#' Returns a list with `text` and a named integer vector `counts`.
#' @noRd
redact_phrase_groups <- function(text_vec, phrase_groups, token) {
  counts <- stats::setNames(integer(length(phrase_groups)), names(phrase_groups))
  entries <- do.call(rbind, lapply(names(phrase_groups), function(group) {
    phrases <- phrase_groups[[group]]
    phrases <- phrases[!is.na(phrases) & nzchar(phrases)]
    if (!length(phrases)) return(NULL)
    data.frame(group = group, phrase = phrases, stringsAsFactors = FALSE)
  }))

  if (is.null(entries) || !nrow(entries)) {
    return(list(text = text_vec, counts = counts))
  }

  entries <- entries[order(nchar(entries$phrase), decreasing = TRUE), ,
                     drop = FALSE]
  seen <- character()
  for (i in seq_len(nrow(entries))) {
    phrase_key <- tolower(entries$phrase[i])
    if (phrase_key %in% seen) next
    seen <- c(seen, phrase_key)

    pat <- boundary_wrap(escape_regex(entries$phrase[i]))
    n_matches <- count_regex_matches(text_vec, pat)
    if (n_matches > 0L) {
      counts[[entries$group[i]]] <- counts[[entries$group[i]]] + n_matches
      text_vec <- gsub(pat, paste0("\\1", token, "\\3"), text_vec,
                       perl = TRUE, ignore.case = TRUE)
    }
  }

  list(text = text_vec, counts = counts)
}

#' Build grouped phrases for counted redaction
#' @noRd
build_redaction_phrase_groups <- function(interviewers,
                                          interviewees,
                                          redact_interviewer,
                                          other_phrases) {
  list(
    participant = unique(c(interviewees,
                           expand_name_parts(interviewees, min_chars = 3))),
    interviewer = if (isTRUE(redact_interviewer)) {
      unique(c(interviewers,
               expand_name_parts(interviewers, min_chars = 3)))
    } else character(),
    other = other_phrases
  )
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

#' Extract the normalized speaker label at the start of a transcript line
#'
#' Detects labels written by `leading_speaker_label()`, including plain
#' `Interviewer:` / `Participant:` labels and VTT voice tags.
#' @noRd
turn_speaker_label <- function(line) {
  if (length(line) != 1 || is.na(line) || !nzchar(trimws(line))) {
    return(NA_character_)
  }

  line <- trimws(line)
  voice <- regexec("^<\\s*v(?:\\.[^>]*)?\\s+(Interviewer|Participant)>",
                   line, perl = TRUE, ignore.case = TRUE)
  voice_match <- regmatches(line, voice)[[1]]
  if (length(voice_match) > 1) {
    return(tolower(voice_match[2]))
  }

  plain <- regexec("^(Interviewer|Participant)\\s*[:\\-]",
                   line, perl = TRUE, ignore.case = TRUE)
  plain_match <- regmatches(line, plain)[[1]]
  if (length(plain_match) > 1) {
    return(tolower(plain_match[2]))
  }

  NA_character_
}

#' Count normalized speaker labels used as turn indexes
#' @noRd
count_speaker_index_labels <- function(lines, speaker) {
  if (!length(lines)) return(0L)
  labels <- vapply(lines, turn_speaker_label, character(1))
  sum(labels == tolower(speaker), na.rm = TRUE)
}

#' Add blank lines only where the detected speaker changes
#' @noRd
add_blanks_between_speaker_changes <- function(lines) {
  if (!length(lines)) return(lines)

  out <- character()
  previous_speaker <- NA_character_
  for (line in lines) {
    current_speaker <- turn_speaker_label(line)
    if (!is.na(current_speaker) &&
        !is.na(previous_speaker) &&
        !identical(current_speaker, previous_speaker)) {
      out <- c(out, "")
    }
    out <- c(out, line)
    if (!is.na(current_speaker)) {
      previous_speaker <- current_speaker
    }
  }
  out
}

#' Remove a normalized leading speaker label from continuation text
#' @noRd
strip_turn_speaker_label <- function(line) {
  if (length(line) != 1 || is.na(line)) return(line)

  line <- sub("^\\s*<\\s*v(?:\\.[^>]*)?\\s+(Interviewer|Participant)>\\s*",
              "", line, perl = TRUE, ignore.case = TRUE)
  sub("^\\s*(Interviewer|Participant)\\s*[:\\-]\\s*",
      "", line, perl = TRUE, ignore.case = TRUE)
}

#' Collapse adjacent lines that belong to the same normalized speaker
#'
#' Keeps the first speaker label and appends later same-speaker lines without
#' repeating the speaker label. Stores the number of collapsed lines in the
#' `optimizations` attribute and per-speaker optimization attributes.
#' @noRd
collapse_consecutive_speaker_lines <- function(lines) {
  if (length(lines) < 2) {
    attr(lines, "optimizations") <- 0L
    attr(lines, "interviewer_optimizations") <- 0L
    attr(lines, "participant_optimizations") <- 0L
    return(lines)
  }

  collapsed <- character()
  optimizations <- 0L
  interviewer_optimizations <- 0L
  participant_optimizations <- 0L
  previous_speaker <- NA_character_
  for (line in lines) {
    current_speaker <- turn_speaker_label(line)
    if (!is.na(current_speaker) &&
        !is.na(previous_speaker) &&
        identical(current_speaker, previous_speaker) &&
        length(collapsed)) {
      continuation <- strip_turn_speaker_label(line)
      if (!is.na(continuation) && nzchar(trimws(continuation))) {
        collapsed[length(collapsed)] <- paste(collapsed[length(collapsed)],
                                             continuation)
      }
      optimizations <- optimizations + 1L
      if (identical(current_speaker, "interviewer")) {
        interviewer_optimizations <- interviewer_optimizations + 1L
      } else if (identical(current_speaker, "participant")) {
        participant_optimizations <- participant_optimizations + 1L
      }
    } else {
      collapsed <- c(collapsed, line)
      if (!is.na(current_speaker)) {
        previous_speaker <- current_speaker
      } else {
        previous_speaker <- NA_character_
      }
    }
  }

  attr(collapsed, "optimizations") <- optimizations
  attr(collapsed, "interviewer_optimizations") <- interviewer_optimizations
  attr(collapsed, "participant_optimizations") <- participant_optimizations
  collapsed
}

#' Collapse adjacent VTT cues that belong to the same normalized speaker
#'
#' Keeps the first cue's id and timestamp, then appends continuation text from
#' later same-speaker cues without repeating the speaker label. Stores the
#' number of collapsed cues in the `optimizations` attribute and per-speaker
#' optimization attributes.
#' @noRd
collapse_consecutive_speaker_cues <- function(cues) {
  if (length(cues) < 2) {
    attr(cues, "optimizations") <- 0L
    attr(cues, "interviewer_optimizations") <- 0L
    attr(cues, "participant_optimizations") <- 0L
    return(cues)
  }

  collapsed <- list()
  optimizations <- 0L
  interviewer_optimizations <- 0L
  participant_optimizations <- 0L
  previous_speaker <- NA_character_
  for (cue in cues) {
    first_line <- if (length(cue$text)) cue$text[1] else NA_character_
    current_speaker <- turn_speaker_label(first_line)

    if (!is.na(current_speaker) &&
        !is.na(previous_speaker) &&
        identical(current_speaker, previous_speaker) &&
        length(collapsed)) {
      continuation <- cue$text
      if (length(continuation)) {
        continuation[1] <- strip_turn_speaker_label(continuation[1])
      }
      last_idx <- length(collapsed)
      collapsed[[last_idx]]$text <- c(collapsed[[last_idx]]$text,
                                      continuation)
      optimizations <- optimizations + 1L
      if (identical(current_speaker, "interviewer")) {
        interviewer_optimizations <- interviewer_optimizations + 1L
      } else if (identical(current_speaker, "participant")) {
        participant_optimizations <- participant_optimizations + 1L
      }
    } else {
      collapsed[[length(collapsed) + 1]] <- cue
      if (!is.na(current_speaker)) {
        previous_speaker <- current_speaker
      } else {
        previous_speaker <- NA_character_
      }
    }
  }

  attr(collapsed, "optimizations") <- optimizations
  attr(collapsed, "interviewer_optimizations") <- interviewer_optimizations
  attr(collapsed, "participant_optimizations") <- participant_optimizations
  collapsed
}

#' Report redacted phrases
#' @noRd
report_redaction_summary <- function(found_names, found_other) {
  if (length(found_names)) {
    message("Names redacted: ", paste(found_names, collapse = ", "))
  }
  if (length(found_other)) {
    message("Other phrases redacted: ", paste(found_other, collapse = ", "))
  }
}

#' Define redaction report count fields
#' @noRd
redaction_report_definitions <- function() {
  data.frame(
    term = c("pre_int_name",
             "post_int_name",
             "post_int_optimization",
             "post_int_name_other",
             "pre_part_name",
             "post_part_name",
             "post_part_optimization",
             "post_part_name_other",
             "other_redactions"),
    definition = c(
      "Number of interviewer name occurrences found in the original input before processing.",
      "Number of times Interviewer appears as a speaker index after processing.",
      "Number of times interviewer turns were collapsed during optimization.",
      "Number of interviewer name occurrences found outside the speaker index and replaced after processing.",
      "Number of participant name occurrences found in the original input before processing.",
      "Number of times Participant appears as a speaker index after processing.",
      "Number of times participant turns were collapsed during optimization.",
      "Number of participant name occurrences found outside the speaker index and replaced after processing.",
      "Number of occurrences replaced from redact_other."
    ),
    stringsAsFactors = FALSE
  )
}

#' Build a one-row redaction report data.frame
#' @noRd
build_redaction_report <- function(redaction_counts,
                                   original_interviewer_names,
                                   original_participant_names,
                                   post_interviewer_index,
                                   post_participant_index,
                                   post_interviewer_optimizations = 0L,
                                   post_participant_optimizations = 0L) {
  report <- data.frame(
    pre_int_name = original_interviewer_names,
    post_int_name = post_interviewer_index,
    post_int_optimization = post_interviewer_optimizations,
    post_int_name_other = unname(redaction_counts[["interviewer"]] %||% 0L),
    pre_part_name = original_participant_names,
    post_part_name = post_participant_index,
    post_part_optimization = post_participant_optimizations,
    post_part_name_other = unname(redaction_counts[["participant"]] %||% 0L),
    other_redactions = unname(redaction_counts[["other"]] %||% 0L),
    stringsAsFactors = FALSE
  )
  attr(report, "definitions") <- redaction_report_definitions()
  report
}

#' Convert a one-row redaction report into a readable table
#' @noRd
format_redaction_report <- function(report) {
  definitions <- attr(report, "definitions", exact = TRUE)
  if (!is.data.frame(definitions)) {
    definitions <- redaction_report_definitions()
  }
  counts <- data.frame(
    term = names(report),
    count = as.integer(unlist(report[1, , drop = TRUE], use.names = FALSE)),
    stringsAsFactors = FALSE
  )
  definitions$count <- counts$count[match(definitions$term, counts$term)]
  out <- definitions[c("term", "definition", "count")]
  out$count <- as.character(out$count)
  out
}

#' Print the formatted redaction report with left-aligned columns
#' @noRd
print_redaction_report <- function(report) {
  display <- format_redaction_report(report)
  widths <- vapply(display, function(col) {
    max(nchar(c(names(col), col)), na.rm = TRUE)
  }, integer(1))
  widths <- pmax(widths, nchar(names(display)))

  format_row <- function(values) {
    paste(mapply(function(value, width) {
      sprintf(paste0("%-", width, "s"), value)
    }, values, widths, USE.NAMES = FALSE), collapse = "  ")
  }

  cat(format_row(names(display)), "\n", sep = "")
  for (i in seq_len(nrow(display))) {
    cat(format_row(unname(unlist(display[i, ], use.names = FALSE))), "\n",
        sep = "")
  }
}

#' Default value helper
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
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

  other_all <- unique(redact_other)

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

# ---- Local name-review helpers ---------------------------------------------

#' Launch a Shiny app in the default R/IDE viewer
#' @noRd
launch_external_browser <- function(url) {
  utils::browseURL(url)
  invisible(TRUE)
}

#' Extract regex matches from a character vector
#' @noRd
extract_regex_matches <- function(text, pattern, ignore.case = FALSE) {
  matches <- gregexpr(pattern, text, perl = TRUE, ignore.case = ignore.case)
  out <- regmatches(text, matches)
  unique(trimws(unlist(out, use.names = FALSE)))
}

#' Normalize a candidate name span
#' @noRd
clean_name_candidate <- function(x) {
  x <- gsub(
    "^\\s*(Name|Patient|Client|Participant|Subject|Speaker|Interviewer|Interviewee|Respondent|Signed by|Signature|Parent|Guardian|Contact)\\s*[:\\-]\\s*",
    "", x, perl = TRUE, ignore.case = TRUE
  )
  x <- gsub("^\\s*(Dr|Prof|Mr|Mrs|Ms|Mx|Miss)\\.?\\s+", "", x,
            perl = TRUE, ignore.case = TRUE)
  x <- gsub("\\s*(MD|PhD|PsyD|LCSW|RN|NP|Jr|Sr|II|III|IV)\\.?\\s*$", "",
            x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("(['’]s)\\b", "", x, perl = TRUE, ignore.case = TRUE)
  trimws(gsub("^[[:punct:][:space:]]+|[[:punct:][:space:]]+$", "", x,
              perl = TRUE))
}

#' Drop leading sentence words from a candidate span
#' @noRd
strip_leading_common_candidate_words <- function(candidate) {
  leading_common <- c(
    "a", "also", "although", "and", "as", "at", "because", "before",
    "but", "by", "during", "for", "from", "however", "if", "in",
    "into", "of", "on", "or", "since", "so", "than", "then",
    "there", "though", "through", "to", "until", "when", "where",
    "while", "with", "without", "this", "that", "these", "those",
    "it", "he", "she", "they", "we", "you", "i", "yes", "no",
    "yeah", "okay", "ok", "um", "uh"
  )

  parts <- unlist(strsplit(candidate, "\\s+", perl = TRUE), use.names = FALSE)
  clean_parts <- tolower(gsub("^[[:punct:]]+|[[:punct:]]+$", "", parts,
                              perl = TRUE))
  while (length(parts) > 1L && clean_parts[1] %in% leading_common) {
    parts <- parts[-1]
    clean_parts <- clean_parts[-1]
  }

  trimws(paste(parts, collapse = " "))
}

#' Score a candidate name span
#' @noRd
score_name_candidate <- function(candidate, text) {
  escaped <- escape_regex(candidate)
  count <- sum(grepl(escaped, text, perl = TRUE, ignore.case = TRUE))
  token_count <- length(strsplit(candidate, "\\s+", perl = TRUE)[[1]])

  score <- 0L
  score <- score + min(count, 3L)
  score <- score + if (token_count >= 2L) 2L else 0L
  score <- score + if (grepl("[-'’]", candidate, perl = TRUE)) 1L else 0L
  score <- score + if (grepl("[^[:ascii:]]", candidate, perl = TRUE)) 1L else 0L
  score
}

#' Identify common capitalized words that should not be name candidates
#' @noRd
is_common_word_candidate <- function(candidate) {
  common_words <- c(
    "a", "an", "and", "are", "as", "at", "because", "but", "by",
    "for", "from", "had", "has", "have", "he", "her", "hers", "him",
    "his", "how", "however", "i", "if", "in", "is", "it", "its",
    "me", "my", "no", "not", "of", "ok", "okay", "on", "or", "our",
    "ours", "she", "so", "that", "the", "their", "theirs", "them",
    "then", "there", "these", "they", "this", "those", "to", "uh",
    "um", "us", "was", "we", "were", "what", "when", "where",
    "which", "while", "who", "why", "with", "yeah", "yes", "you",
    "your", "yours"
  )

  tokens <- tolower(unlist(strsplit(candidate, "\\s+", perl = TRUE),
                           use.names = FALSE))
  tokens <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", tokens, perl = TRUE)
  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) return(TRUE)

  if (length(tokens) == 1L && tokens %in% common_words) return(TRUE)
  all(tokens %in% common_words)
}

#' Find likely person names using local rules only
#' @noRd
find_likely_names <- function(text, min_score = 2) {
  name_token <- "\\p{Lu}\\p{L}+(?:[-'’]\\p{Lu}?\\p{L}+)?"
  particle <- "(?:al|bin|binti|da|de|del|della|der|di|dos|du|el|la|le|van|von)"
  name_piece <- paste0("(?:", name_token, "|", particle, ")")
  multi_name <- paste0("\\b", name_token, "(?:\\s+", name_piece, "){1,4}\\b")

  title_name <- paste0(
    "\\b(?:Dr|Prof|Mr|Mrs|Ms|Mx|Miss)\\.?\\s+",
    name_token,
    "(?:\\s+", name_piece, "){0,4}\\b"
  )
  label_name <- paste0(
    "\\b(?:Name|Patient|Client|Participant|Subject|Speaker|Interviewer|",
    "Interviewee|Respondent|Signed by|Signature|Parent|Guardian|Contact)",
    "\\s*[:\\-]\\s+",
    name_token,
    "(?:\\s+", name_piece, "){0,4}\\b"
  )
  speaker_name <- paste0("^\\s*", name_token, "(?:\\s+", name_piece, "){0,4}\\s*[:\\-]")

  candidates <- c(
    extract_regex_matches(text, label_name, ignore.case = TRUE),
    extract_regex_matches(text, title_name, ignore.case = TRUE),
    extract_regex_matches(text, speaker_name, ignore.case = FALSE),
    extract_regex_matches(text, multi_name, ignore.case = FALSE)
  )

  candidates <- unname(vapply(candidates, clean_name_candidate, character(1)))
  candidates <- unname(vapply(candidates, strip_leading_common_candidate_words,
                              character(1)))
  candidates <- unique(candidates)
  candidates <- candidates[nzchar(candidates)]

  stop_terms <- c(
    "WEBVTT", "Interviewer", "Interviewee", "Participant", "Subject",
    "Patient", "Client", "Monday", "Tuesday", "Wednesday", "Thursday",
    "Friday", "Saturday", "Sunday", "January", "February", "March", "April",
    "May", "June", "July", "August", "September", "October", "November",
    "December", "New York", "United States"
  )
  candidates <- candidates[!(tolower(candidates) %in% tolower(stop_terms))]
  candidates <- candidates[!vapply(candidates, is_common_word_candidate,
                                   logical(1))]

  scores <- unname(vapply(candidates, score_name_candidate, integer(1),
                          text = text))
  out <- data.frame(candidate = candidates, score = scores,
                    stringsAsFactors = FALSE)
  out <- out[out$score >= min_score, , drop = FALSE]
  out[order(tolower(out$candidate), out$candidate), , drop = FALSE]
}

#' Parse manually entered redaction terms
#' @noRd
parse_manual_redact_terms <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character())
  terms <- unlist(strsplit(x, "[\r\n,;]+", perl = TRUE), use.names = FALSE)
  terms <- trimws(terms)
  unique(terms[nzchar(terms)])
}

#' Review candidate names in a local Shiny app
#' @noRd
review_name_candidates <- function(candidates,
                                   known_interviewers = character(),
                                   known_interviewees = character()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The shiny package is required for the checkbox review app. ",
      "Install it with install.packages('shiny')."
    )
  }

  if (!nrow(candidates)) {
    candidates <- data.frame(candidate = character(), score = integer(),
                             stringsAsFactors = FALSE)
  }
  candidates$id <- seq_len(nrow(candidates))
  nyu_purple <- "#57068C"

  candidate_rows <- if (nrow(candidates)) {
    lapply(seq_len(nrow(candidates)), function(i) {
      shiny::tags$tr(
        shiny::tags$td(
          shiny::tags$strong(candidates$candidate[i]),
          shiny::tags$span(class = "score",
                           paste0("score ", candidates$score[i]))
        ),
        shiny::tags$td(
          shiny::checkboxInput(paste0("interviewer_", i), NULL, value = FALSE)
        ),
        shiny::tags$td(
          shiny::checkboxInput(paste0("participant_", i), NULL, value = FALSE)
        ),
        shiny::tags$td(
          shiny::checkboxInput(paste0("other_", i), NULL, value = FALSE)
        )
      )
    })
  } else {
    list(shiny::tags$tr(
      shiny::tags$td(colspan = 4, class = "empty",
                     "No likely names were detected. Add other terms below if needed.")
    ))
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0(
        "
        body {
          font-family: Cambria, Georgia, serif;
          max-width: 960px;
          margin: 0 auto;
          padding: 24px;
          color: #1f1f1f;
          background: #fafafa;
        }
        h2 {
          color: ", nyu_purple, ";
          font-weight: 700;
        }
        .btn-primary {
          background-color: ", nyu_purple, ";
          border-color: ", nyu_purple, ";
        }
        .btn-primary:hover,
        .btn-primary:focus {
          background-color: #41046b;
          border-color: #41046b;
        }
        .review-panel {
          padding: 16px;
          border-left: 5px solid ", nyu_purple, ";
          background: #ffffff;
          overflow-x: auto;
          box-sizing: border-box;
        }
        .warning-panel {
          background: #f7f2fa;
          border-left-color: ", nyu_purple, ";
          margin-bottom: 16px;
        }
        table.review-table {
          width: 100%;
          min-width: 620px;
          border-collapse: collapse;
          table-layout: fixed;
        }
        .review-table th:first-child,
        .review-table td:first-child {
          width: 46%;
        }
        .review-table th:not(:first-child),
        .review-table td:not(:first-child) {
          width: 18%;
        }
        .review-table th {
          color: ", nyu_purple, ";
          border-bottom: 2px solid ", nyu_purple, ";
          padding: 8px;
          text-align: center;
        }
        .review-table th:first-child,
        .review-table td:first-child {
          text-align: left;
        }
        .review-table td {
          border-bottom: 1px solid #e5e5e5;
          padding: 8px;
          text-align: center;
          vertical-align: middle;
          overflow-wrap: anywhere;
        }
        .review-table .checkbox {
          margin: 0;
        }
        .score {
          display: block;
          color: #666666;
          font-size: 12px;
          margin-top: 2px;
        }
        .empty {
          color: #666666;
          padding: 18px;
        }
        .actions {
          margin-top: 20px;
          display: flex;
          gap: 8px;
        }
        textarea {
          min-height: 120px;
        }
        "
      )))
    ),
    shiny::titlePanel("Review Likely Names"),
    shiny::div(
      class = "review-panel warning-panel",
      shiny::tags$strong("Important: "),
      "The candidate list below is based on regex rules for text that looks ",
      "like names. It can miss names and can include false positives. The ",
      "rules look for patterns such as capitalized words, ",
      "speaker labels, and titles such as Dr., Mr., Ms., and Miss. ",
      shiny::tags$strong("Strong recommendation: "),
      "The safest way to redact while preserving speaker indexing is to list all ",
      "known interviewer and participant names in the spaces below, keeping an eye on how participant names",
      "appear in transcript files. If Zoom usernames differ from formal names, for example, this list may not",
      "catch these cases" ,
      "Ease your heart knowing that this app does not use AI and runs locally on your computer."
    ),
    shiny::hr(),
    shiny::div(
      class = "review-panel",
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textAreaInput(
            inputId = "manual_interviewers",
            label = "Known interviewer names",
            value = paste(known_interviewers, collapse = "\n"),
            placeholder = "Enter one interviewer name per line."
          )
        ),
        shiny::column(
          width = 6,
          shiny::textAreaInput(
            inputId = "manual_interviewees",
            label = "Known participant names",
            value = paste(known_interviewees, collapse = "\n"),
            placeholder = "Enter one participant name per line."
          )
        )
      ),
      shiny::hr(),
      shiny::tags$table(
        class = "review-table",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Candidate"),
            shiny::tags$th("Interviewer"),
            shiny::tags$th("Participant"),
            shiny::tags$th("Other")
          )
        ),
        shiny::tags$tbody(candidate_rows)
      ),
      shiny::hr(),
      shiny::textAreaInput(
        inputId = "manual_other",
        label = "Other terms to redact",
        value = "",
        placeholder = "Enter one term per line, or separate terms with commas/semicolons."
      )
    ),
    shiny::div(
      class = "actions",
      shiny::actionButton("done", "Apply selections", class = "btn-primary"),
      shiny::actionButton("cancel", "Cancel")
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      checked <- function(prefix) {
        if (!nrow(candidates)) return(character())
        values <- vapply(seq_len(nrow(candidates)), function(i) {
          isTRUE(input[[paste0(prefix, "_", i)]])
        }, logical(1))
        candidates$candidate[values]
      }

      shiny::stopApp(list(
        interviewers = unique(c(checked("interviewer"),
                                parse_manual_redact_terms(input$manual_interviewers))),
        interviewees = unique(c(checked("participant"),
                                parse_manual_redact_terms(input$manual_interviewees))),
        other = unique(c(checked("other"),
                         parse_manual_redact_terms(input$manual_other)))
      ))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(list(
        interviewers = character(),
        interviewees = character(),
        other = character()
      ))
    })
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server),
                launch.browser = launch_external_browser)
}

#' Add reviewed candidates to the role-aware redaction arguments
#' @noRd
review_redaction_terms <- function(text,
                                   interviewers,
                                   interviewees,
                                   redact_other,
                                   review_names,
                                   min_score) {
  out <- list(
    interviewers = interviewers,
    interviewees = interviewees,
    redact_other = redact_other
  )

  if (!isTRUE(review_names)) return(out)

  candidates <- find_likely_names(text, min_score = min_score)
  approved <- review_name_candidates(candidates, interviewers, interviewees)

  out$interviewers <- unique(c(out$interviewers, approved$interviewers))
  out$interviewees <- unique(c(out$interviewees, approved$interviewees))
  out$redact_other <- unique(c(out$redact_other, approved$other))
  out
}

#' Show a local completion notice after redaction finishes
#' @noRd
show_redaction_complete <- function(show_notice,
                                    message = "Your transcript(s) are ready for you!",
                                    report = NULL) {
  if (!isTRUE(show_notice)) return(invisible(NULL))

  if (!requireNamespace("shiny", quietly = TRUE)) {
    message(message)
    if (is.data.frame(report)) {
      print(report)
    }
    return(invisible(NULL))
  }

  report_tables <- completion_report_tables(report)

  nyu_purple <- "#57068C"
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0(
        "
        body {
          font-family: Cambria, Georgia, serif;
          max-width: 720px;
          margin: 0 auto;
          padding: 48px 24px;
          background: #fafafa;
          color: #1f1f1f;
        }
        .complete-box {
          border-left: 6px solid #198754;
          background: #d1e7dd;
          color: #0f5132;
          padding: 24px;
          font-size: 20px;
          font-weight: 700;
        }
        .btn-primary {
          background-color: ", nyu_purple, ";
          border-color: ", nyu_purple, ";
          margin-top: 20px;
        }
        .btn-primary:hover,
        .btn-primary:focus {
          background-color: #41046b;
          border-color: #41046b;
        }
        .report-panel {
          margin-top: 24px;
          padding: 16px;
          background: #ffffff;
          border-left: 5px solid ", nyu_purple, ";
          overflow-x: auto;
        }
        .report-panel h3 {
          color: ", nyu_purple, ";
          margin-top: 0;
        }
        .report-panel table {
          width: 100%;
          border-collapse: collapse;
        }
        .report-panel th {
          color: ", nyu_purple, ";
          border-bottom: 2px solid ", nyu_purple, ";
          padding: 8px;
          text-align: left;
        }
        .report-panel td {
          border-bottom: 1px solid #e5e5e5;
          padding: 8px;
          vertical-align: top;
        }
        "
      )))
    ),
    shiny::div(class = "complete-box", message),
    if (is.data.frame(report_tables$summary) && nrow(report_tables$summary)) {
      shiny::div(
        class = "report-panel",
        shiny::tags$h3(report_tables$summary_title),
        shiny::tableOutput("summary_report_table")
      )
    },
    if (is.data.frame(report_tables$detail) && nrow(report_tables$detail)) {
      shiny::div(
        class = "report-panel",
        shiny::tags$h3(report_tables$detail_title),
        shiny::tableOutput("detail_report_table")
      )
    },
    shiny::actionButton("close", "Close", class = "btn-primary")
  )

  server <- function(input, output, session) {
    if (is.data.frame(report_tables$summary) && nrow(report_tables$summary)) {
      output$summary_report_table <- shiny::renderTable(report_tables$summary,
                                                        striped = TRUE,
                                                        bordered = FALSE,
                                                        spacing = "s")
    }

    if (is.data.frame(report_tables$detail) && nrow(report_tables$detail)) {
      output$detail_report_table <- shiny::renderTable(report_tables$detail,
                                                       striped = TRUE,
                                                       bordered = FALSE,
                                                       spacing = "s")
    }

    shiny::observeEvent(input$close, {
      shiny::stopApp(invisible(NULL))
    })
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server),
                launch.browser = launch_external_browser)
  invisible(NULL)
}

#' Format report data for the completion app
#' @noRd
completion_report_tables <- function(report) {
  empty <- list(
    summary = data.frame(),
    detail = data.frame(),
    summary_title = "Redaction report",
    detail_title = "Per-transcript report"
  )

  if (!is.data.frame(report) || !nrow(report)) {
    return(empty)
  }

  report_cols <- redaction_report_definitions()$term
  if (all(report_cols %in% names(report)) && nrow(report) == 1) {
    empty$summary <- format_redaction_report(report)
    return(empty)
  }

  if (all(report_cols %in% names(report))) {
    totals <- data.frame(
      pre_int_name = sum(report$pre_int_name, na.rm = TRUE),
      post_int_name = sum(report$post_int_name, na.rm = TRUE),
      post_int_optimization = sum(report$post_int_optimization, na.rm = TRUE),
      post_int_name_other = sum(report$post_int_name_other, na.rm = TRUE),
      pre_part_name = sum(report$pre_part_name, na.rm = TRUE),
      post_part_name = sum(report$post_part_name, na.rm = TRUE),
      post_part_optimization = sum(report$post_part_optimization, na.rm = TRUE),
      post_part_name_other = sum(report$post_part_name_other, na.rm = TRUE),
      other_redactions = sum(report$other_redactions, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    attr(totals, "definitions") <- redaction_report_definitions()

    detail_cols <- c("output_file", report_cols)
    keep <- detail_cols[detail_cols %in% names(report)]
    detail <- report[, keep, drop = FALSE]
    names(detail) <- sub("_", " ", names(detail), fixed = TRUE)

    empty$summary <- format_redaction_report(totals)
    empty$detail <- detail
    empty$summary_title <- "Summary across transcripts"
    empty$detail_title <- "Per-transcript report"
    return(empty)
  }

  empty$summary <- report
  empty
}
