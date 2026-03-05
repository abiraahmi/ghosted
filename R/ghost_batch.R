#' Batch redact transcripts in a folder
#'
#' Scans a folder for transcript files and runs the appropriate redaction
#' function per file type:
#' - `.docx` -> [ghost_docx()]
#' - `.txt`  -> [ghost_txt()]
#' - `.vtt`  -> [ghost_vtt()] (can output VTT/DOCX/TXT)
#'
#' Outputs are written to `output_dir` (defaults to `input_dir`) using the input
#' base filename with `suffix` appended before the extension. Shows a console
#' progress bar while processing.
#'
#' @param input_dir Folder containing transcripts.
#' @param interviewers Character vector of interviewer names.
#' @param interviewees Character vector of interviewee/participant names.
#' @param output_dir Folder to write outputs (default: `input_dir`).
#' @param recursive If `TRUE`, include files in subdirectories.
#' @param suffix Suffix to append to each output filename (default: `_redacted`).
#' @param out_format Output format for all inputs; one of `"vtt"`, `"docx"`, or
#'   `"txt"`. If `NULL` (default), each file keeps its original format. If you
#'   request `"vtt"` for `.docx`/`.txt` inputs, cues are written without
#'   timestamps.
#' @param redact_other Other words/phrases to redact.
#' @param redact_interviewer If `TRUE`, also redact interviewer names.
#' @param include_common_names If `TRUE`, also redact top US baby names (uses
#'   `common_names_fun`).
#' @param common_names_fun Function used when `include_common_names = TRUE`
#'   (default: top 1000 US baby names from `babynames`).
#' @param report_redacted If `TRUE`, print phrases found/redacted per file.
#' @param name_token Replacement token for names.
#' @param school_token Replacement token for schools/other phrases.
#' @param add_blank_line_between_turns For VTT -> DOCX/TXT outputs, insert a
#'   blank line between turns.
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
                        output_dir = NULL,
                        recursive = FALSE,
                        suffix = "_redacted",
                        out_format = NULL,
                        redact_other = character(),
                        redact_interviewer = FALSE,
                        include_common_names = FALSE,
                        common_names_fun = NULL,
                        report_redacted = FALSE,
                        name_token = "[REDACTED]",
                        school_token = "[REDACTED]",
                        add_blank_line_between_turns = TRUE) {

  if (!is.character(input_dir) || length(input_dir) != 1 || !nzchar(input_dir)) {
    stop("Provide a single 'input_dir' path")
  }
  if (!dir.exists(input_dir)) stop("Directory not found: ", input_dir)
  if (is.null(output_dir)) output_dir <- input_dir
  if (!base::dir.exists(output_dir)) base::dir.create(output_dir, recursive = TRUE)

  fmt <- if (is.null(out_format)) NULL else match.arg(out_format, c("vtt","docx","txt"))

  # Collect files (case-insensitive)
  files <- base::list.files(input_dir,
                      pattern = "\\.(docx|txt|vtt)$",
                      full.names = TRUE,
                      recursive = isTRUE(recursive),
                      ignore.case = TRUE)
  if (!length(files)) {
    warning("No .docx, .txt, or .vtt files found in ", input_dir)
    return(invisible(data.frame(input_file = character(), output_file = character(), status = character())))
  }

  n_files <- length(files)
  results <- vector("list", n_files)
  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_files, style = 3)
  on.exit({ try(close(pb), silent = TRUE) }, add = TRUE)

  for (i in seq_along(files)) {
    f <- files[i]
    ext <- tolower(tools::file_ext(f))
    base <- tools::file_path_sans_ext(base::basename(f))
    out_file <- NA_character_
    status <- "ok"

    # Compute default output path
    # Decide output extension per requested format (or keep original when auto)
    ext_out <- if (is.null(fmt)) paste0(".", ext) else switch(fmt, vtt = ".vtt", docx = ".docx", txt = ".txt")
    out_path <- base::file.path(output_dir, paste0(base, suffix, ext_out))

    # Process per type
    tryCatch({
      if (identical(ext, "vtt")) {
        # Use ghost_vtt; let out_format decide extension
        out_file <- ghost_vtt(
          filepath = f,
          interviewers = interviewers,
          interviewees = interviewees,
          output_dir = output_dir,
          out_format = if (is.null(fmt)) "vtt" else fmt,
          suffix = suffix,
          redact_other = redact_other,
          redact_interviewer = redact_interviewer,
          include_common_names = include_common_names,
          common_names_fun = common_names_fun,
          report_redacted = report_redacted,
          name_token = name_token,
          school_token = school_token,
          add_blank_line_between_turns = add_blank_line_between_turns
        )
      } else if (identical(ext, "docx")) {
        # If DOCX requested, delegate to ghost_docx; otherwise convert
        if (is.null(fmt) || fmt == "docx") {
          out_file <- ghost_docx(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = out_path,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
        } else if (!is.null(fmt) && fmt == "txt") {
          # Redact to temp DOCX then convert paragraphs to TXT
          tmp <- base::tempfile(fileext = ".docx")
          ghost_docx(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = tmp,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
          ds <- officer::docx_summary(officer::read_docx(tmp))
          if ("content_type" %in% names(ds)) ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
          lines <- if (nrow(ds)) {
            if ("paragraph_id" %in% names(ds)) {
              sp <- split(ds$text, ds$paragraph_id)
              unname(vapply(sp, function(v) paste(v[!is.na(v)], collapse = ""), character(1)))
            } else { ds$text }
          } else character()
          con <- base::file(out_path, open = "w", encoding = "UTF-8")
          base::writeLines(lines, con, sep = "\n", useBytes = TRUE)
          base::close(con)
          out_file <- out_path
        } else if (!is.null(fmt) && fmt == "vtt") {
          # Redact to temp DOCX then convert paragraphs to VTT cues without timestamps
          tmp <- base::tempfile(fileext = ".docx")
          ghost_docx(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = tmp,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
          ds <- officer::docx_summary(officer::read_docx(tmp))
          if ("content_type" %in% names(ds)) ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
          paras <- if (nrow(ds)) {
            if ("paragraph_id" %in% names(ds)) {
              sp <- split(ds$text, ds$paragraph_id)
              unname(vapply(sp, function(v) paste(v[!is.na(v)], collapse = ""), character(1)))
            } else { ds$text }
          } else character()
          con <- base::file(out_path, open = "w", encoding = "UTF-8")
          base::writeLines("WEBVTT", con); base::writeLines("", con)
          ncue <- length(paras)
          for (k in base::seq_len(ncue)) {
            base::writeLines(c(base::as.character(k),
                         ifelse(base::is.na(paras[k]), "", paras[k]),
                         ""), con)
          }
          base::close(con); out_file <- out_path
        }
      } else if (identical(ext, "txt")) {
        if (is.null(fmt) || fmt == "txt") {
          out_file <- ghost_txt(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = out_path,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
        } else if (!is.null(fmt) && fmt == "docx") {
          # Redact to temp TXT then convert to DOCX paragraphs
          tmp <- base::tempfile(fileext = ".txt")
          ghost_txt(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = tmp,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
          lines <- base::tryCatch(base::readLines(tmp, warn = FALSE, encoding = "UTF-8"), error = function(e) base::character())
          doc <- officer::read_docx()
          if (length(lines)) {
            for (ln in lines) {
              doc <- officer::body_add_par(doc, ifelse(is.na(ln), "", ln), style = "Normal")
              if (add_blank_line_between_turns) doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          print(doc, target = out_path)
          out_file <- out_path
        } else if (!is.null(fmt) && fmt == "vtt") {
          # Redact to temp TXT then convert to VTT cues without timestamps
          tmp <- base::tempfile(fileext = ".txt")
          ghost_txt(
            filepath = f,
            interviewers = interviewers,
            interviewees = interviewees,
            output_path = tmp,
            redact_other = redact_other,
            redact_interviewer = redact_interviewer,
            include_common_names = include_common_names,
            common_names_fun = common_names_fun,
            report_redacted = report_redacted,
            name_token = name_token,
            school_token = school_token
          )
          lines <- base::tryCatch(base::readLines(tmp, warn = FALSE, encoding = "UTF-8"), error = function(e) base::character())
          con <- base::file(out_path, open = "w", encoding = "UTF-8")
          base::writeLines("WEBVTT", con); base::writeLines("", con)
          ncue <- base::length(lines)
          for (k in base::seq_len(ncue)) {
            base::writeLines(c(base::as.character(k),
                         ifelse(base::is.na(lines[k]), "", lines[k]),
                         ""), con)
          }
          base::close(con); out_file <- out_path
        }
      } else {
        status <- paste0("skipped (unknown ext: .", ext, ")")
      }
    }, error = function(e) {
      status <<- paste0("error: ", conditionMessage(e))
      out_file <<- NA_character_
    })

    results[[i]] <- data.frame(
      input_file = f,
      output_file = ifelse(is.character(out_file) && length(out_file) == 1, out_file, NA_character_),
      status = status,
      stringsAsFactors = FALSE
    )

    # Update progress
    utils::setTxtProgressBar(pb, i)
  }

  out <- do.call(rbind, results)
  invisible(out)
}
