#' Redact transcripts using a local app
#'
#' Opens a local Shiny app that asks for an input directory, output directory,
#' known interviewer names, known participant names, and other terms to redact.
#' The app scans supported transcript files in the input directory, shows likely
#' names for review, and writes redacted transcript files. Supported input
#' types are `.docx`, `.txt`, and `.vtt`.
#'
#' @param input_dir Optional folder containing transcripts. If `NULL`, the app
#'   asks for it.
#' @param output_dir Optional folder to write outputs. If `NULL`, the app asks
#'   for it, defaulting to `input_dir` when left blank.
#' @param interviewers Optional character vector of interviewer names to prefill
#'   in the app.
#' @param interviewees Optional character vector of interviewee/participant names
#'   to prefill in the app.
#' @param redact_other Optional terms to prefill in the app's other-redaction
#'   field.
#' @param redact_interviewer Default value for the app option that controls
#'   whether interviewer names are redacted in body text.
#' @param include_common_names Default value for the app option that includes
#'   the package's default common-name list when available.
#' @param redacted_token Default replacement token shown in the app.
#' @param add_blank_line_between_turns Default value for the app option that
#'   inserts a blank line between turns for DOCX/TXT outputs.
#' @param recursive Default value for the app option to include subdirectories.
#' @param suffix Default suffix shown in the app for output filenames.
#' @param out_format Default output format shown in the app. One of `"vtt"`,
#'   `"docx"`, or `"txt"`. If `NULL`, the app defaults to keeping original
#'   formats.
#' @param report_redacted Default value for the app option that prints
#'   redaction summaries to the R console.
#' @param name_review_min_score Minimum rule-based score for a candidate to
#'   appear in the review app.
#' @param show_completion_notice Default value for the app option that opens a
#'   local completion notice after all outputs are written.
#' @return A data.frame with `input_file`, `output_file`, `status`, and redaction
#'   report columns. Invisibly returned.
#' @examples
#' # In an interactive R session:
#' # ghost()
#' @export
ghost <- function(input_dir = NULL,
                  output_dir = NULL,
                  interviewers = character(),
                  interviewees = character(),
                  redact_other = character(),
                  redact_interviewer = TRUE,
                  include_common_names = FALSE,
                  redacted_token = "[REDACTED]",
                  add_blank_line_between_turns = TRUE,
                  recursive = FALSE,
                  suffix = "_redacted",
                  out_format = NULL,
                  report_redacted = FALSE,
                  name_review_min_score = 2,
                  show_completion_notice = TRUE) {

  preload_notice <- is.list(ghost_app_state$values)
  previous <- ghost_app_previous_values()
  if (missing(input_dir)) input_dir <- previous$input_dir
  if (missing(output_dir)) output_dir <- previous$output_dir
  if (missing(interviewers)) interviewers <- previous$interviewers
  if (missing(interviewees)) interviewees <- previous$interviewees
  if (missing(redact_other)) redact_other <- previous$redact_other
  if (missing(redact_interviewer)) {
    redact_interviewer <- previous$redact_interviewer
  }
  if (missing(include_common_names)) {
    include_common_names <- previous$include_common_names
  }
  if (missing(redacted_token)) redacted_token <- previous$redacted_token
  if (missing(add_blank_line_between_turns)) {
    add_blank_line_between_turns <- previous$add_blank_line_between_turns
  }
  if (missing(recursive)) recursive <- previous$recursive
  if (missing(suffix)) suffix <- previous$suffix
  if (missing(out_format)) out_format <- previous$out_format
  if (missing(report_redacted)) report_redacted <- previous$report_redacted
  if (missing(show_completion_notice)) {
    show_completion_notice <- previous$show_completion_notice
  }

  fmt <- if (is.null(out_format)) NULL else match.arg(out_format,
                                                      c("vtt", "docx", "txt"))

  app_values <- ghost_review_app(
    input_dir = input_dir,
    output_dir = output_dir,
    interviewers = interviewers,
    interviewees = interviewees,
    redact_other = redact_other,
    redact_interviewer = redact_interviewer,
    include_common_names = include_common_names,
    redacted_token = redacted_token,
    add_blank_line_between_turns = add_blank_line_between_turns,
    recursive = recursive,
    suffix = suffix,
    out_format = fmt,
    report_redacted = report_redacted,
    show_completion_notice = show_completion_notice,
    preload_notice = preload_notice,
    name_review_min_score = name_review_min_score
  )

  input_dir <- app_values$input_dir
  output_dir <- app_values$output_dir
  interviewers <- app_values$interviewers
  interviewees <- app_values$interviewees
  redact_other <- app_values$redact_other
  redact_interviewer <- app_values$redact_interviewer
  include_common_names <- app_values$include_common_names
  redacted_token <- app_values$redacted_token
  add_blank_line_between_turns <- app_values$add_blank_line_between_turns
  recursive <- app_values$recursive
  suffix <- app_values$suffix
  fmt <- app_values$out_format
  report_redacted <- app_values$report_redacted
  show_completion_notice <- app_values$show_completion_notice
  ghost_app_save_values(app_values)

  ghost_batch(
    input_dir = input_dir,
    interviewers = interviewers,
    interviewees = interviewees,
    redact_other = redact_other,
    redact_interviewer = redact_interviewer,
    include_common_names = include_common_names,
    redacted_token = redacted_token,
    add_blank_line_between_turns = add_blank_line_between_turns,
    output_dir = output_dir,
    recursive = recursive,
    suffix = suffix,
    out_format = fmt,
    report_redacted = report_redacted,
    review_names = FALSE,
    name_review_min_score = name_review_min_score,
    show_completion_notice = show_completion_notice
  )
}

#' Run the combined setup and likely-name review app for ghost()
#' @noRd
ghost_review_app <- function(input_dir = NULL,
                             output_dir = NULL,
                             interviewers = character(),
                             interviewees = character(),
                             redact_other = character(),
                             redact_interviewer = TRUE,
                             include_common_names = FALSE,
                             redacted_token = "[REDACTED]",
                             add_blank_line_between_turns = TRUE,
                             recursive = FALSE,
                             suffix = "_redacted",
                             out_format = NULL,
                             report_redacted = FALSE,
                             show_completion_notice = TRUE,
                             preload_notice = FALSE,
                             name_review_min_score = 2) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The shiny package is required for ghost(). ",
      "Install it with install.packages('shiny')."
    )
  }

  nyu_purple <- "#57068C"
  output_choice <- if (is.null(out_format)) "original" else out_format

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(paste0(
        "
        body {
          font-family: Cambria, Georgia, serif;
          max-width: 1040px;
          margin: 0 auto;
          padding: 24px;
          color: #1f1f1f;
          background: #fafafa;
        }
        h2, h3 {
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
          margin-bottom: 16px;
        }
        .intro-panel {
          background: #f7f2fa;
        }
        .name-review-note {
          background: #f7f2fa;
          border-left: 5px solid ", nyu_purple, ";
          padding: 12px 14px;
          margin: 0 0 16px 0;
        }
        .name-review-note p {
          margin: 0;
        }
        .status {
          color: #555555;
          margin-top: 8px;
        }
        .required {
          color: #b00020;
          font-weight: 700;
        }
        .section-actions {
          display: flex;
          justify-content: flex-end;
          margin-top: 4px;
        }
        .help-text {
          color: #666666;
          font-size: 13px;
          line-height: 1.35;
          margin: -8px 0 12px 0;
        }
        .checkbox-help {
          margin: -4px 0 14px 20px;
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
          text-align: left;
        }
        .review-table th:not(:first-child),
        .review-table td:not(:first-child) {
          width: 18%;
          text-align: center;
        }
        .review-table th {
          color: ", nyu_purple, ";
          border-bottom: 2px solid ", nyu_purple, ";
          padding: 8px;
        }
        .review-table td {
          border-bottom: 1px solid #e5e5e5;
          padding: 8px;
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
          min-height: 110px;
        }
        .option-row {
          align-items: flex-end;
        }
        "
      )))
    ),
    shiny::titlePanel("You're about to ghost some folks \U0001F47B"),
    if (isTRUE(preload_notice)) {
      shiny::div(
        class = "review-panel intro-panel",
        shiny::tags$p(
          shiny::tags$strong("Heads up! "),
          "If you're re-running the app on the same batch of transcripts that live in the same input directory and want the redacted transcripts exported to the same output directory, we've preloaded this for you. ",
          "We also remembered the selections you made the last time you reviewed likely names. ",
          "If it's a different batch of transcripts, please update the section(s) below!"
        )
      )
    },
    shiny::div(
      class = "review-panel",
      shiny::tags$h3("Directories"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            "input_dir",
            shiny::tagList("Input directory ",
                           shiny::tags$span(class = "required", "*")),
            value = if (is.null(input_dir)) "" else input_dir,
            placeholder = "Folder containing .docx, .txt, or .vtt transcripts"
          ),
          shiny::div(class = "help-text",
                     "Folder containing transcripts to redact.")
        ),
        shiny::column(
          width = 6,
          shiny::textInput(
            "output_dir",
            "Output directory",
            value = if (is.null(output_dir)) "" else output_dir,
            placeholder = "Folder for redacted files; blank uses input directory"
          ),
          shiny::div(class = "help-text",
                     "Folder to write redacted outputs. Leave blank to use the input directory.")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectInput(
            "out_format",
            "Output format",
            choices = c("Keep original formats" = "original",
                        "Word (.docx)" = "docx",
                        "Text (.txt)" = "txt",
                        "WebVTT (.vtt)" = "vtt"),
            selected = output_choice
          ),
          shiny::div(class = "help-text",
                     "Keep each file's original format, or convert every output to DOCX, TXT, or VTT. DOCX/TXT to VTT outputs do not include timestamps.")
        ),
        shiny::column(
          width = 4,
          shiny::checkboxInput("recursive", "Include subdirectories",
                               value = isTRUE(recursive)),
          shiny::div(class = "help-text checkbox-help",
                     "Scan transcript files inside nested folders too.")
        )
      ),
      shiny::div(
        class = "section-actions",
        shiny::actionButton("scan", "Scan directory", class = "btn-primary")
      ),
      shiny::uiOutput("scan_status")
    ),
    shiny::div(
      class = "review-panel",
      shiny::tags$h3("Options"),
      shiny::fluidRow(
        class = "option-row",
        shiny::column(
          width = 4,
          shiny::textInput("suffix", "Output filename suffix",
                           value = suffix),
          shiny::div(class = "help-text",
                     "Added to each filename before the extension, such as sample_redacted.docx.")
        ),
        shiny::column(
          width = 4,
          shiny::textInput("redacted_token", "Redaction token",
                           value = redacted_token),
          shiny::div(class = "help-text",
                     "Replacement text used for redacted names and other phrases.")
        ),
        shiny::column(
          width = 4,
          shiny::checkboxInput("add_blank_line_between_turns",
                               "Add blank lines between turns",
                               value = isTRUE(add_blank_line_between_turns)),
          shiny::div(class = "help-text checkbox-help",
                     "For DOCX and TXT outputs, insert a blank line between speaker turns.")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::checkboxInput("redact_interviewer",
                               "Redact interviewer names in text",
                               value = isTRUE(redact_interviewer)),
          shiny::div(class = "help-text checkbox-help",
                     "Replace interviewer names when they appear in transcript body text. Speaker labels are still normalized to Interviewer.")
        ),
        shiny::column(
          width = 3,
          shiny::checkboxInput("include_common_names",
                               "Include common names",
                               value = isTRUE(include_common_names)),
          shiny::div(class = "help-text checkbox-help",
                     "Also redact the package's default common-name list when that data is available.")
        ),
        shiny::column(
          width = 3,
          shiny::checkboxInput("report_redacted",
                               "Print console report",
                               value = isTRUE(report_redacted)),
          shiny::div(class = "help-text checkbox-help",
                     "Print per-file redaction summaries in the R console while processing.")
        ),
        shiny::column(
          width = 3,
          shiny::checkboxInput("show_completion_notice",
                               "Show completion notice",
                               value = isTRUE(show_completion_notice)),
          shiny::div(class = "help-text checkbox-help",
                     "Open a local completion screen with a summary report after outputs are written.")
        )
      )
    ),
    shiny::div(
      class = "review-panel intro-panel",
      shiny::tags$p(
        shiny::tags$strong("One quick note before we begin: "),
        "The safest way to redact while preserving speaker indexing is to list all ",
        "known interviewer and participant names in the spaces below, keeping an eye on how participant names ",
        "appear in transcript files. If Zoom usernames differ from formal names, for example, this list may not ",
        "catch these cases. ",
        "Ease your heart knowing that this app does not use AI and runs locally on your computer."
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::textAreaInput(
            "manual_interviewers",
            "Known interviewer names",
            value = paste(interviewers, collapse = "\n"),
            placeholder = "Enter one interviewer name per line."
          ),
          shiny::div(class = "help-text",
                     "Names listed here are used to normalize interviewer speaker labels and, when selected above, redact interviewer names in body text.")
        ),
        shiny::column(
          width = 4,
          shiny::textAreaInput(
            "manual_interviewees",
            "Known participant names",
            value = paste(interviewees, collapse = "\n"),
            placeholder = "Enter one participant name per line."
          ),
          shiny::div(class = "help-text",
                     "Names listed here are used to normalize participant speaker labels and redact participant names in body text.")
        ),
        shiny::column(
          width = 4,
          shiny::textAreaInput(
            "manual_other",
            "Other terms to redact",
            value = paste(redact_other, collapse = "\n"),
            placeholder = "Enter one term per line, or separate terms with commas/semicolons."
          ),
          shiny::div(class = "help-text",
                     "Additional exact words or phrases to redact across all transcripts.")
        )
      )
    ),
    shiny::div(
      class = "review-panel",
      shiny::tags$h3("Review Likely Names"),
      shiny::div(
        class = "help-text",
        "These candidates are detected locally with rule-based matching. Check Interviewer, Participant, or Other to add a candidate to the corresponding redaction list for every transcript in the batch."
      ),
      shiny::div(
        class = "name-review-note",
        shiny::tags$p(
          shiny::tags$strong("Multi-word candidates: "),
          "Interviewer and Participant redact both the full name and each name part wherever it appears. For example, selecting 'Sansa Stark' also redacts 'Sansa' and 'Stark' separately. Other redacts only the complete phrase 'Sansa Stark'."
        )
      ),
      shiny::uiOutput("candidate_table")
    ),
    shiny::div(
      class = "actions",
      shiny::actionButton("done", "Redact transcripts", class = "btn-primary"),
      shiny::actionButton("cancel", "Cancel")
    )
  )

  server <- function(input, output, session) {
    candidates <- shiny::reactiveVal(data.frame(candidate = character(),
                                                score = integer(),
                                                stringsAsFactors = FALSE))
    scan_message <- shiny::reactiveVal("")

    scan_inputs <- function() {
      dir <- trimws(input$input_dir %||% "")
      if (!nzchar(dir)) {
        scan_message("Input directory is required.")
        candidates(data.frame(candidate = character(), score = integer(),
                              stringsAsFactors = FALSE))
        return(FALSE)
      }
      if (!dir.exists(path.expand(dir))) {
        scan_message(paste0("Directory not found: ", dir))
        candidates(data.frame(candidate = character(), score = integer(),
                              stringsAsFactors = FALSE))
        return(FALSE)
      }

      files <- ghost_transcript_files(dir, recursive = isTRUE(input$recursive))
      if (!length(files)) {
        scan_message(paste0("No .docx, .txt, or .vtt files found in ", dir))
        candidates(data.frame(candidate = character(), score = integer(),
                              stringsAsFactors = FALSE))
        return(FALSE)
      }

      found <- find_likely_names(collect_batch_review_text(files),
                                 min_score = name_review_min_score)
      candidates(found)
      scan_message(paste0("Found ", length(files), " transcript file",
                          if (length(files) == 1) "" else "s", " and ",
                          nrow(found), " likely name",
                          if (nrow(found) == 1) "" else "s", "."))
      TRUE
    }

    output$scan_status <- shiny::renderUI({
      if (!nzchar(scan_message())) return(NULL)
      shiny::div(class = "status", scan_message())
    })

    output$candidate_table <- shiny::renderUI({
      found <- candidates()
      if (!nrow(found)) {
        return(shiny::tags$div(
          class = "empty",
          "No likely names are listed yet. Scan a directory, or add known names above."
        ))
      }

      preload_interviewers <- unique(interviewers)
      preload_interviewees <- unique(interviewees)
      preload_other <- unique(redact_other)

      rows <- lapply(seq_len(nrow(found)), function(i) {
        candidate <- found$candidate[i]
        shiny::tags$tr(
          shiny::tags$td(
            shiny::tags$strong(candidate),
            shiny::tags$span(class = "score",
                             paste0("score ", found$score[i]))
          ),
          shiny::tags$td(
            shiny::checkboxInput(paste0("interviewer_", i), NULL,
                                 value = candidate %in% preload_interviewers)
          ),
          shiny::tags$td(
            shiny::checkboxInput(paste0("participant_", i), NULL,
                                 value = candidate %in% preload_interviewees)
          ),
          shiny::tags$td(
            shiny::checkboxInput(paste0("other_", i), NULL,
                                 value = candidate %in% preload_other)
          )
        )
      })

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
        shiny::tags$tbody(rows)
      )
    })

    shiny::observeEvent(input$scan, {
      scan_inputs()
    })

    shiny::observeEvent(input$done, {
      if (!scan_inputs()) return(invisible(NULL))

      found <- candidates()
      checked <- function(prefix) {
        if (!nrow(found)) return(character())
        selected <- vapply(seq_len(nrow(found)), function(i) {
          isTRUE(input[[paste0(prefix, "_", i)]])
        }, logical(1))
        found$candidate[selected]
      }

      selected_format <- input$out_format
      if (identical(selected_format, "original")) selected_format <- NULL

      out_dir <- trimws(input$output_dir %||% "")
      if (!nzchar(out_dir)) out_dir <- trimws(input$input_dir %||% "")
      selected_suffix <- trimws(input$suffix %||% "")
      if (!nzchar(selected_suffix)) selected_suffix <- "_redacted"
      selected_token <- input$redacted_token %||% ""
      if (!nzchar(selected_token)) selected_token <- "[REDACTED]"

      shiny::stopApp(list(
        input_dir = trimws(input$input_dir %||% ""),
        output_dir = out_dir,
        interviewers = unique(c(
          checked("interviewer"),
          parse_manual_redact_terms(input$manual_interviewers)
        )),
        interviewees = unique(c(
          checked("participant"),
          parse_manual_redact_terms(input$manual_interviewees)
        )),
        redact_other = unique(c(
          checked("other"),
          parse_manual_redact_terms(input$manual_other)
        )),
        redact_interviewer = isTRUE(input$redact_interviewer),
        include_common_names = isTRUE(input$include_common_names),
        redacted_token = selected_token,
        add_blank_line_between_turns =
          isTRUE(input$add_blank_line_between_turns),
        recursive = isTRUE(input$recursive),
        suffix = selected_suffix,
        out_format = selected_format,
        report_redacted = isTRUE(input$report_redacted),
        show_completion_notice = isTRUE(input$show_completion_notice)
      ))
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp(structure(list(), class = "ghost_cancelled"))
    })
  }

  values <- shiny::runApp(shiny::shinyApp(ui = ui, server = server),
                          launch.browser = launch_external_browser)
  if (inherits(values, "ghost_cancelled")) {
    stop("ghost() cancelled.", call. = FALSE)
  }
  values
}

#' List supported transcript files for the ghost app
#' @noRd
ghost_transcript_files <- function(input_dir, recursive = FALSE) {
  list.files(path.expand(input_dir),
             pattern = "\\.(docx|txt|vtt)$",
             full.names = TRUE,
             recursive = isTRUE(recursive),
             ignore.case = TRUE)
}

#' Store ghost app values for reuse within the current R session
#' @noRd
ghost_app_state <- new.env(parent = emptyenv())

#' Default values used when no app state has been saved
#' @noRd
ghost_app_default_values <- function() {
  list(
    input_dir = NULL,
    output_dir = NULL,
    interviewers = character(),
    interviewees = character(),
    redact_other = character(),
    redact_interviewer = TRUE,
    include_common_names = FALSE,
    redacted_token = "[REDACTED]",
    add_blank_line_between_turns = TRUE,
    recursive = FALSE,
    suffix = "_redacted",
    out_format = NULL,
    report_redacted = FALSE,
    show_completion_notice = TRUE
  )
}

#' Get app values saved from the previous successful ghost() run
#' @noRd
ghost_app_previous_values <- function() {
  defaults <- ghost_app_default_values()
  saved <- ghost_app_state$values
  if (!is.list(saved)) return(defaults)

  for (nm in intersect(names(defaults), names(saved))) {
    defaults[[nm]] <- saved[[nm]]
  }
  defaults
}

#' Save app values after a successful ghost() review step
#' @noRd
ghost_app_save_values <- function(values) {
  defaults <- ghost_app_default_values()
  keep <- intersect(names(defaults), names(values))
  saved <- defaults
  for (nm in keep) {
    saved[[nm]] <- values[[nm]]
  }
  ghost_app_state$values <- saved
  invisible(saved)
}
