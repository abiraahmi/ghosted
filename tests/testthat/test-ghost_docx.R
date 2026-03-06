test_that("ghost_docx relabels leading speakers and redacts elsewhere", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")

  in_doc <- tempfile(fileext = ".docx")
  out_doc <- tempfile(fileext = ".docx")

  d <- officer::read_docx()
  d <- officer::body_add_par(d, "Kailey Rivera: Hello Kailey", style = "Normal")
  d <- officer::body_add_par(d, "Alex Baloney: Hi Kailey", style = "Normal")
  print(d, target = in_doc)

  res <- ghost_docx(
    filepath = in_doc,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_path = out_doc
  )
  expect_true(file.exists(res))

  ds <- officer::docx_summary(officer::read_docx(res))
  if ("content_type" %in% names(ds)) ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
  paras <- if (nrow(ds)) {
    if ("paragraph_id" %in% names(ds)) {
      sp <- split(ds$text, ds$paragraph_id)
      unname(vapply(sp, function(v) paste(v[!is.na(v)], collapse = ""), character(1)))
    } else ds$text
  } else character()

  expect_true(any(grepl("^Interviewer:", paras)))
  expect_true(any(grepl("^Participant:", paras)))
  # Names not at start should be redacted
  expect_false(any(grepl("Kailey", paras)))
})

test_that("ghost_docx handles empty documents (no paragraphs)", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")
  infile <- tempfile(fileext = ".docx")
  outfile <- tempfile(fileext = ".docx")
  # Create an empty docx (no body content)
  print(officer::read_docx(), target = infile)

  res <- ghost_docx(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_path = outfile
  )
  expect_true(file.exists(res))
})
test_that("ghost_docx default output path and reporting", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")
  td <- tempfile("gdocx_", fileext = ""); dir.create(td)
  infile <- file.path(td, "s.docx")

  d <- officer::read_docx()
  d <- officer::body_add_par(d, "<Kailey Rivera> Hello Dragon", style = "Normal")
  d <- officer::body_add_par(d, "[Alex Baloney] Hi there", style = "Normal")
  print(d, target = infile)

  res <- ghost_docx(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_path = NULL,
    redact_other = "Dragon",
    include_common_names = FALSE,
    report_redacted = TRUE
  )
  expect_true(file.exists(res))
  expect_match(res, "s_redacted\\.docx$")

  ds <- officer::docx_summary(officer::read_docx(res))
  if ("content_type" %in% names(ds)) ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
  paras <- if (nrow(ds)) {
    if ("paragraph_id" %in% names(ds)) {
      sp <- split(ds$text, ds$paragraph_id)
      unname(vapply(sp, function(v) paste(v[!is.na(v)], collapse = ""), character(1)))
    } else ds$text
  } else character()
  expect_true(any(grepl("Interviewer|Participant", paras)))
  expect_false(any(grepl("Dragon", paras)))
})


test_that("ghost_docx can write TXT with suffix and custom token", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")

  td <- tempfile("gdocx_txt_", fileext = ""); dir.create(td)
  infile <- file.path(td, "in.docx")
  outfile <- file.path(td, "in_OUT.txt")

  d <- officer::read_docx()
  d <- officer::body_add_par(d, "Kailey Rivera: Hello Dragon", style = "Normal")
  d <- officer::body_add_par(d, "Alex Baloney: Bye", style = "Normal")
  print(d, target = infile)

  res <- ghost_docx(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    redact_other = "Dragon",
    redacted_token = "[X]",
    out_format = "txt",
    output_path = outfile
  )
  expect_true(file.exists(res))
  out <- readLines(res, warn = FALSE)
  # Should have blank lines inserted between turns
  expect_true(any(out == ""))
  # Leading labels and custom token applied
  expect_true(any(grepl("^Interviewer", out)))
  expect_false(any(grepl("Kailey|Alex|Dragon", out, ignore.case = TRUE)))
  expect_true(any(grepl("\\[X\\]", out)))
})

test_that("ghost_docx can write VTT with header and labels", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")

  td <- tempfile("gdocx_vtt_", fileext = ""); dir.create(td)
  infile <- file.path(td, "in.docx")
  vtt_out <- file.path(td, "in_VTT.vtt")

  d <- officer::read_docx()
  d <- officer::body_add_par(d, "Kailey Rivera: Hello Dragon", style = "Normal")
  d <- officer::body_add_par(d, "[Alex Baloney] Bye", style = "Normal")
  print(d, target = infile)

  res <- ghost_docx(
    filepath = infile,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    redact_other = "Dragon",
    out_format = "vtt",
    output_path = vtt_out
  )
  expect_true(file.exists(res))
  got <- readLines(res, warn = FALSE)
  expect_identical(trimws(got[1]), "WEBVTT")
  expect_true(any(grepl("^1$", got)))
  expect_true(any(grepl("Interviewer|Participant", got)))
  expect_false(any(grepl("Kailey|Alex|Dragon", got, ignore.case = TRUE)))
})
