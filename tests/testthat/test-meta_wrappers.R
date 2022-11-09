
test_that2("meta wrappers", {
  expect_error(
    obj <- meta_string(
      varname = "Species",
      label = "Iris species",
      tags = c("tag1", "tag2")
    ),
    regexp = NA
  )
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_number(
    varname = "Sepal.Length",
    label = "Sepal length",
    tags = c("tag1", "tag2"),
    digits = 2
  )
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_factor(
    varname = "Species",
    label = "Iris species",
    tags = c("tag1", "tag2"),
    levels = c("setosa", "versicolor", "virginica")
  )
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_date(varname = "date")
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_datetime(varname = "var1")
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_graph(varname = "var1", idvarname = "idvar", direction = "to")
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_geo(varname = "var1", latvar = "lat", longvar = "long")
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")

  obj <- meta_href(varname = "var1")
  expect_s3_class(obj, "R6")
  expect_s3_class(obj, "trelliscope_meta_def")
})
