test_that("ghost_vtt preserves timestamps and relabels voice tags", {

  vtt <- tempfile(fileext = ".vtt")
  out <- tempfile(fileext = ".vtt")
  lines <- c(
    "WEBVTT",
    "",
    "1",
    "00:00:00.000 --> 00:00:01.000",
    "<v Kailey Rivera> Thanks Alex",
    "",
    "2",
    "00:00:01.500 --> 00:00:02.500",
    "<v [Alex Baloney]> Hi there",
    "",
    "3",
    "00:00:03.000 --> 00:00:04.000",
    "[Alex Baloney]: Another from Kailey",
    "",
    "4",
    "00:00:04.500 --> 00:00:05.500",
    "Alex Baloney: Hello",
    ""
  )
  writeLines(lines, vtt, useBytes = TRUE)

  res <- ghost_vtt(
    filepath = vtt,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_dir = dirname(out),
    out_format = "vtt",
    suffix = "_t"
  )
  expect_true(file.exists(res))
  got <- readLines(res, warn = FALSE)

  # Voice tags normalized
  expect_true(any(grepl("<v Interviewer>", got)))
  expect_true(any(grepl("<v Participant>", got)))
  # Names elsewhere are redacted
  expect_false(any(grepl("Kailey Rivera|Alex Baloney", got)))
  # Timestamps preserved
  expect_true(any(grepl("-->", got)))
})

test_that("ghost_vtt writes txt and relabels multiple patterns", {
  td <- tempfile("gvtt_", fileext = ""); dir.create(td)
  infile <- file.path(td, "m.vtt")
  lines <- c(
    "WEBVTT", "",
    "00:00:00.000 --> 00:00:01.000",
    "<v [Alex Baloney]> Hi", "",
    "2", "00:00:01.000 --> 00:00:02.000", "[Kailey Rivera]: Hello Alex", "",
    "3", "00:00:02.000 --> 00:00:03.000", "<Kailey Rivera> Test", "",
    "4", "00:00:03.000 --> 00:00:04.000", "Alex Baloney: Done", ""
  )
  writeLines(lines, infile, useBytes = TRUE)

  res <- ghost_vtt(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    out_format = "txt",
    output_dir = td,
    suffix = "_out"
  )
  expect_true(file.exists(res))
  out <- readLines(res, warn = FALSE)
  # Ensure Participant/Interviewer labels appear
  expect_true(any(grepl("Participant|Interviewer", out)))
})

test_that("ghost_vtt can write docx when officer available", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")
  td <- tempfile("gvtt2_", fileext = ""); dir.create(td)
  infile <- file.path(td, "n.vtt")
  writeLines(c(
    "WEBVTT", "",
    "1", "00:00:00.000 --> 00:00:01.000", "<v Kailey Rivera> Hello", ""
  ), infile, useBytes = TRUE)
  res <- ghost_vtt(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    out_format = "docx",
    output_dir = td,
    suffix = "_d"
  )
  expect_true(file.exists(res))
})

test_that("ghost_vtt report_redacted emits messages", {
  vtt <- tempfile(fileext = ".vtt")
  writeLines(c(
    "WEBVTT", "",
    "1", "00:00:00.000 --> 00:00:01.000", "<v Kailey Rivera> Hello Dragon", "",
    "2", "00:00:01.000 --> 00:00:02.000", "Alex Baloney: Bye", ""
  ), vtt, useBytes = TRUE)

  expect_silent({
    # run once to create file
    ghost_vtt(vtt, interviewers = "Kailey Rivera", interviewees = "Alex Baloney",
              output_dir = tempdir(), out_format = "vtt", suffix = "_r")
  })

  # Now with report_redacted = TRUE and other phrase set
  expect_message(
    ghost_vtt(vtt,
              interviewers = "Kailey Rivera",
              interviewees = "Alex Baloney",
              redact_other = "Dragon",
              report_redacted = TRUE,
              output_dir = tempdir(), out_format = "vtt", suffix = "_r2"),
    regexp = "Names redacted|Other phrases redacted"
  )
})

