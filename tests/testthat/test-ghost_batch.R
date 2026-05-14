test_that("ghost_batch processes a mix of files and writes outputs", {

  td <- tempfile("gb_", fileext = "")
  dir.create(td, recursive = TRUE)
  outd <- file.path(td, "out"); dir.create(outd)

  # Prepare TXT
  txt <- file.path(td, "a.txt")
  writeLines(c("Kailey Rivera: hello", "Alex Baloney: hi"), txt, useBytes = TRUE)

  # Prepare VTT
  vtt <- file.path(td, "b.vtt")
  writeLines(c(
    "WEBVTT", "",
    "1", "00:00:00.000 --> 00:00:01.000", "<v Kailey Rivera> Hello", "",
    "2", "00:00:01.000 --> 00:00:02.000", "Alex Baloney: Hi", ""
  ), vtt, useBytes = TRUE)

  # Prepare DOCX (skip if officer missing)
  docx <- NULL
  if (requireNamespace("officer", quietly = TRUE)) {
    docx <- file.path(td, "c.docx")
    d <- officer::read_docx(); d <- officer::body_add_par(d, "Kailey Rivera: hello", style = "Normal")
    print(d, target = docx)
  }

  res <- ghost_batch(
    input_dir    = td,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_dir   = outd,
    suffix       = "_DEID",
    out_format   = NULL
  )
  expect_s3_class(res, "data.frame")
  # Expect at least two results (vtt + txt), three if officer available
  expect_true(nrow(res) >= 2)
  expect_true(all(c("pre_int_name",
                    "post_int_name",
                    "post_int_optimization",
                    "post_int_name_other",
                    "pre_part_name",
                    "post_part_name",
                    "post_part_optimization",
                    "post_part_name_other",
                    "other_redactions") %in% names(res)))
  definitions <- attr(res, "redaction_report_definitions", exact = TRUE)
  expect_s3_class(definitions, "data.frame")
  expect_true(all(c("term", "definition") %in% names(definitions)))
  expect_false("label" %in% names(definitions))
  # All reported output paths exist
  existing <- res$output_file[!is.na(res$output_file)]
  expect_true(all(file.exists(existing)))

  # Now convert everything to VTT (txt->vtt path without timestamps)
  res2 <- ghost_batch(
    input_dir    = td,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_dir   = outd,
    suffix       = "_VTT",
    out_format   = "vtt"
  )
  expect_true(all(grepl("_VTT\\.vtt$", res2$output_file[!is.na(res2$output_file)])))

  # Convert text to DOCX if officer available
  if (requireNamespace("officer", quietly = TRUE)) {
    res3 <- ghost_batch(
      input_dir    = td,
      interviewers = "Kailey Rivera",
      interviewees = "Alex Baloney",
      output_dir   = outd,
      suffix       = "_DOCX",
      out_format   = "docx"
    )
    expect_true(any(grepl("_DOCX\\.docx$", na.omit(res3$output_file))))
  }
})

test_that("ghost_batch covers conversions and common names", {
  td <- tempfile("gbm_", fileext = ""); dir.create(td)
  outd <- file.path(td, "out"); dir.create(outd)

  # TXT with names and a common phrase
  txt <- file.path(td, "t.txt"); writeLines(c("Kailey Rivera: Dragon", "Alex Baloney: ok"), txt)

  # VTT minimal
  vtt <- file.path(td, "v.vtt"); writeLines(c("WEBVTT", "", "1", "00:00:00.000 --> 00:00:01.000", "<v Kailey Rivera> Dragon", ""), vtt)

  # DOCX minimal if officer is present
  docx <- NULL
  if (requireNamespace("officer", quietly = TRUE)) {
    docx <- file.path(td, "d.docx")
    d <- officer::read_docx(); d <- officer::body_add_par(d, "Kailey Rivera: Dragon", style = "Normal"); print(d, target = docx)
  }

  # Convert everything to TXT to hit docx->txt and vtt->txt paths
  res_txt <- ghost_batch(
    input_dir    = td,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    redact_other = "Dragon",
    output_dir   = outd,
    out_format   = "txt",
    suffix       = "_C",
    include_common_names = FALSE
  )
  expect_true(all(grepl("_C\\.txt$", na.omit(res_txt$output_file))))
})

test_that("ghost_batch adds blank Word paragraphs for docx inputs", {
  if (!requireNamespace("officer", quietly = TRUE)) skip("officer not installed")
  td <- tempfile("gb_docx_blanks_", fileext = ""); dir.create(td)
  outd <- file.path(td, "out"); dir.create(outd)

  infile <- file.path(td, "sample.docx")
  d <- officer::read_docx()
  d <- officer::body_add_par(d, "Kailey Rivera: Hello", style = "Normal")
  d <- officer::body_add_par(d, "Alex Baloney: Hi", style = "Normal")
  print(d, target = infile)

  res <- ghost_batch(
    input_dir = td,
    interviewers = "Kailey Rivera",
    interviewees = "Alex Baloney",
    output_dir = outd,
    out_format = NULL,
    add_blank_line_between_turns = TRUE
  )

  expect_identical(res$status, "ok")
  ds <- officer::docx_summary(officer::read_docx(res$output_file))
  if ("content_type" %in% names(ds)) {
    ds <- ds[ds$content_type == "paragraph", , drop = FALSE]
  }
  expect_true(any(ds$text == "", na.rm = TRUE))
})

test_that("ghost_batch passes strict redact_other behavior through handlers", {
  td <- tempfile("gb_strict_", fileext = ""); dir.create(td)
  outd <- file.path(td, "out"); dir.create(outd)

  txt <- file.path(td, "t.txt")
  writeLines(c(
    "Alex Baloney: Visit Dragon Fruit today",
    "Dragon should remain for context",
    "Fruit should remain for context"
  ), txt, useBytes = TRUE)

  res <- ghost_batch(
    input_dir = td,
    interviewers = character(),
    interviewees = "Alex Baloney",
    redact_interviewer = FALSE,
    redact_other = "Dragon Fruit",
    output_dir = outd,
    out_format = "txt",
    suffix = "_strict"
  )

  expect_identical(res$status, "ok")
  got <- readLines(res$output_file, warn = FALSE)
  expect_true(any(grepl("\\[REDACTED\\]", got)))
  expect_true(any(grepl("\\bDragon\\b", got)))
  expect_true(any(grepl("\\bFruit\\b", got)))
  expect_false(any(grepl("Dragon Fruit", got, ignore.case = TRUE)))
})
