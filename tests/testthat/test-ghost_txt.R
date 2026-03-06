test_that("ghost_txt relabels leading speakers and redacts elsewhere", {

  txt <- tempfile(fileext = ".txt")
  out <- tempfile(fileext = ".txt")

  lines <- c(
    "Kailey Rivera: Hello Kailey",
    "Alex Baloney: Hi Kailey",
    "No speaker here but mentions Alex and Kailey"
  )
  writeLines(lines, txt, useBytes = TRUE)

  res <- ghost_txt(
    filepath = txt,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_path = out
  )
  expect_true(file.exists(res))
  got <- readLines(out, warn = FALSE)

  expect_true(grepl("^Interviewer:", got[1]))
  expect_true(grepl("\\[REDACTED\\]", got[1]))
  expect_true(grepl("^Participant:", got[2]))
  expect_true(grepl("\\[REDACTED\\]", got[2]))
  # Both names redacted when not at start
  expect_false(any(grepl("Kailey|Alex", got[3], ignore.case = TRUE)))
})

test_that("ghost_txt default path and common names redaction", {
  td <- tempfile("gtxt_", fileext = ""); dir.create(td)
  infile <- file.path(td, "sample.txt")
  # Include BOM and CR to exercise normalization
  lines <- c("\ufeffKailey Rivera - Hello Dragon\r", "[Alex Baloney] Greetings")
  con <- file(infile, open = "wb"); writeChar(paste0(lines, collapse = "\n"), con, eos = NULL); close(con)

  res <- ghost_txt(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_path = NULL,
    redact_other = "Dragon",
    include_common_names = FALSE,
    report_redacted = TRUE
  )
  expect_true(file.exists(res))
  expect_match(res, paste0(basename(tools::file_path_sans_ext(infile)), "_redacted\\.txt$"))
  out <- readLines(res, warn = FALSE)
  # Leading replacements applied
  expect_true(grepl("^Interviewer\\s*-", out[1]))
  expect_true(grepl("^Participant", out[2]))
  # "Dragon" redacted via redact_other
  expect_true(grepl("\\[REDACTED\\]", paste(out, collapse = "\n")))
})

test_that("ghost_txt can write VTT with header and tokens", {
  td <- tempfile("gtxt_vtt_", fileext = ""); dir.create(td)
  infile <- file.path(td, "s.txt")
  writeLines(c("Kailey Rivera: Hello Dragon", "Alex Baloney: Hi"), infile, useBytes = TRUE)

  vtt_out <- file.path(td, "s_redacted.vtt")
  res <- ghost_txt(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    redact_other = "Dragon",
    redacted_token = "[X]",
    out_format = "vtt",
    output_path = vtt_out
  )
  expect_true(file.exists(res))
  got <- readLines(res, warn = FALSE)
  expect_identical(trimws(got[1]), "WEBVTT")
  expect_true(any(grepl("^1$|^2$", got)))
  expect_true(any(grepl("Interviewer|Participant", got)))
  expect_false(any(grepl("Kailey|Alex|Dragon", got, ignore.case = TRUE)))
  expect_true(any(grepl("\\[X\\]", got)))
})

test_that("ghost_txt report_redacted emits messages", {
  td <- tempfile("gtxt_msg_", fileext = ""); dir.create(td)
  infile <- file.path(td, "m.txt")
  writeLines(c("Kailey Rivera: Hello Dragon", "Text mentions Alex"), infile, useBytes = TRUE)

  expect_message(
    ghost_txt(
      filepath = infile,
      interviewers = "Kailey Rivera",
      interviewees = "Alex Baloney",
      redact_other = "Dragon",
      report_redacted = TRUE,
      output_path = file.path(td, "m_out.txt")
    ),
    regexp = "Names redacted|Other phrases redacted"
  )
})
