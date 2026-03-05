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
    output_dir   = outd,
    out_format   = "txt",
    suffix       = "_C",
    include_common_names = TRUE,
    common_names_fun = function() c("Dragon")
  )
  expect_true(all(grepl("_C\\.txt$", na.omit(res_txt$output_file))))
})

