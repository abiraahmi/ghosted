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
    include_common_names = TRUE,
    common_names_fun = function() c("Dragon"),
    report_redacted = TRUE
  )
  expect_true(file.exists(res))
  expect_match(res, paste0(basename(tools::file_path_sans_ext(infile)), "_redacted\\.txt$"))
  out <- readLines(res, warn = FALSE)
  # Leading replacements applied
  expect_true(grepl("^Interviewer\\s*-", out[1]))
  expect_true(grepl("^Participant", out[2]))
  # "Dragon" redacted by common_names_fun
  expect_true(grepl("\\[REDACTED\\]", paste(out, collapse = "\n")))
})

