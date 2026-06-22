test_that("ghost_transcript_files finds supported files only", {
  td <- tempfile("ghost_files_", fileext = "")
  dir.create(td, recursive = TRUE)
  writeLines("a", file.path(td, "a.txt"))
  writeLines("b", file.path(td, "b.vtt"))
  writeLines("c", file.path(td, "c.csv"))

  files <- basename(ghost_transcript_files(td))

  expect_identical(sort(files), c("a.txt", "b.vtt"))
})

test_that("ghost app values are saved and reused within the session", {
  old <- ghost_app_state$values
  on.exit({
    if (is.null(old)) {
      rm(list = "values", envir = ghost_app_state)
    } else {
      ghost_app_state$values <- old
    }
  }, add = TRUE)

  ghost_app_save_values(list(
    input_dir = "input",
    output_dir = "output",
    interviewers = "Inter Viewer",
    interviewees = "Part Icipant",
    redact_other = "Other Term",
    redact_interviewer = FALSE,
    include_common_names = TRUE,
    redacted_token = "[X]",
    add_blank_line_between_turns = FALSE,
    recursive = TRUE,
    suffix = "_ghosted",
    out_format = "docx",
    report_redacted = TRUE,
    show_completion_notice = FALSE
  ))

  values <- ghost_app_previous_values()

  expect_identical(values$input_dir, "input")
  expect_identical(values$output_dir, "output")
  expect_identical(values$interviewers, "Inter Viewer")
  expect_identical(values$interviewees, "Part Icipant")
  expect_identical(values$redact_other, "Other Term")
  expect_false(values$redact_interviewer)
  expect_true(values$include_common_names)
  expect_identical(values$redacted_token, "[X]")
  expect_false(values$add_blank_line_between_turns)
  expect_true(values$recursive)
  expect_identical(values$suffix, "_ghosted")
  expect_identical(values$out_format, "docx")
  expect_true(values$report_redacted)
  expect_false(values$show_completion_notice)
})

test_that("ghost app state reports whether prior values exist", {
  old <- ghost_app_state$values
  on.exit({
    if (is.null(old)) {
      rm(list = "values", envir = ghost_app_state)
    } else {
      ghost_app_state$values <- old
    }
  }, add = TRUE)

  if (exists("values", envir = ghost_app_state, inherits = FALSE)) {
    rm(list = "values", envir = ghost_app_state)
  }
  expect_false(is.list(ghost_app_state$values))

  ghost_app_save_values(list(input_dir = "input"))
  expect_true(is.list(ghost_app_state$values))
})
