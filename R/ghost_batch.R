#' Batch redact transcripts in a folder
#'
#' Scans a folder for transcript files and runs the appropriate redaction
#' function per file type:
#' - `.docx` -> [ghost_docx()]
#' - `.txt`  -> [ghost_txt()]
#' - `.vtt`  -> [ghost_vtt()]
#'
#' Each file is dispatched to its single-file handler with `out_format`, so any
#' format-conversion logic lives in one place (the per-format function).
#' Outputs are written to `output_dir` (defaults to `input_dir`) using the input
#' base filename with `suffix` appended before the extension. Shows a console
#' progress bar while processing.
#'
#' @param input_dir Folder containing transcripts.
#' @param interviewers Character vector of interviewer names.
#' @param interviewees Character vector of interviewee/participant names.
#' @param redact_other Other words/phrases to redact.
#' @param redact_interviewer If `TRUE` (default), redact interviewer names in
#'   body text. Passed through to the per-format functions.
#' @param include_common_names If `TRUE`, also redact a default list of common
#'   names (e.g., top US baby names, if available via
#'   `ghosted::common_names_default`).
#' @param redacted_token Replacement token used for redactions (names and other
#'   phrases).
#' @param add_blank_line_between_turns For DOCX/TXT outputs, insert a blank
#'   line between turns.
#' @param output_dir Folder to write outputs (default: `input_dir`).
#' @param recursive If `TRUE`, include files in subdirectories.
#' @param suffix Suffix to append to each output filename (default: `_redacted`).
#' @param out_format Output format for all inputs; one of `"vtt"`, `"docx"`, or
#'   `"txt"`. If `NULL` (default), each file keeps its original format. When a
#'   `.docx`/`.txt` input is asked for `"vtt"`, cues are written without
#'   timestamps (handled by [ghost_docx()]/[ghost_txt()]).
#' @param report_redacted If `TRUE`, print phrases found/redacted per file.
#' @return A data.frame with `input_file`, `output_file`, and `status` columns.
#'   Invisibly returned.
#' @examples
#' # Redact all transcripts in a folder, keeping the same formats:
#' # ghost_batch("inst/data/transcripts", interviewers = "Dr. Smith",
#' #   interviewees = "Jane Doe", suffix = "_DEID")
#' # Redact VTTs to DOCX, others keep their formats:
#' # ghost_batch("inst/data/transcripts", interviewers = "Dr. Smith",
#' #   interviewees = "Jane Doe", out_format = "docx")
#' @export
ghost_batch <- function(input_dir,
                        interviewers,
                        interviewees = character(),
                        redact_other = character(),
                        redact_interviewer = TRUE,
                        include_common_names = FALSE,
                        redacted_token = "[REDACTED]",
                        add_blank_line_between_turns = TRUE,
                        output_dir = NULL,
                        recursive = FALSE,
                        suffix = "_redacted",
                        out_format = NULL,
                        report_redacted = FALSE) {

  if (!is.character(input_dir) || length(input_dir) != 1 || !nzchar(input_dir)) {
    stop("Provide a single 'input_dir' path")
  }
  if (!dir.exists(input_dir)) stop("Directory not found: ", input_dir)
  if (is.null(output_dir)) output_dir <- input_dir
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  fmt <- if (is.null(out_format)) NULL else match.arg(out_format, c("vtt", "docx", "txt"))

  files <- list.files(input_dir,
                      pattern = "\\.(docx|txt|vtt)$",
                      full.names = TRUE,
                      recursive = isTRUE(recursive),
                      ignore.case = TRUE)
  if (!length(files)) {
    warning("No .docx, .txt, or .vtt files found in ", input_dir)
    return(invisible(empty_batch_result()))
  }

  n_files <- length(files)
  results <- vector("list", n_files)
  pb <- utils::txtProgressBar(min = 0, max = n_files, style = 3)
  on.exit({ try(close(pb), silent = TRUE) }, add = TRUE)

  common_args <- list(
    interviewers                 = interviewers,
    interviewees                 = interviewees,
    redact_other                 = redact_other,
    redact_interviewer           = redact_interviewer,
    include_common_names         = include_common_names,
    redacted_token               = redacted_token,
    add_blank_line_between_turns = add_blank_line_between_turns,
    report_redacted              = report_redacted
  )

  for (i in seq_along(files)) {
    f <- files[i]
    ext <- tolower(tools::file_ext(f))
    stem <- tools::file_path_sans_ext(basename(f))
    target_fmt <- if (is.null(fmt)) ext else fmt
    out_ext <- paste0(".", target_fmt)
    out_path <- file.path(output_dir, paste0(stem, suffix, out_ext))

    out_file <- NA_character_
    status <- "ok"

    handler <- switch(ext,
                      vtt  = ghost_vtt,
                      docx = ghost_docx,
                      txt  = ghost_txt,
                      NULL)

    if (is.null(handler)) {
      status <- paste0("skipped (unknown ext: .", ext, ")")
    } else {
      tryCatch({
        out_file <- do.call(handler, c(
          list(filepath    = f,
               output_path = out_path,
               suffix      = suffix,
               out_format  = target_fmt),
          common_args
        ))
      }, error = function(e) {
        status <<- paste0("error: ", conditionMessage(e))
        out_file <<- NA_character_
      })
    }

    results[[i]] <- data.frame(
      input_file  = f,
      output_file = if (is.character(out_file) && length(out_file) == 1) out_file else NA_character_,
      status      = status,
      stringsAsFactors = FALSE
    )

    utils::setTxtProgressBar(pb, i)
  }

  invisible(do.call(rbind, results))
}

#' Empty result skeleton returned when no files match
#' @noRd
empty_batch_result <- function() {
  data.frame(input_file  = character(),
             output_file = character(),
             status      = character(),
             stringsAsFactors = FALSE)
}
