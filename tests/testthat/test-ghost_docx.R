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
    include_common_names = TRUE,
    common_names_fun = function() c("Dragon"),
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


