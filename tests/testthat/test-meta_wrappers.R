
test_that("meta wrappers", {
  obj <- meta_string(
    varname = "Species",
    label = "Iris species",
    tags = c("tag1", "tag2")
  )

  obj <- meta_number(
    varname = "Sepal.Length",
    label = "Sepal length",
    tags = c("tag1", "tag2"),
    digits = 2
  )

  obj <- meta_factor(
    varname = "Species",
    label = "Iris species",
    tags = c("tag1", "tag2"),
    levels = c("setosa", "versicolor", "virginica")
  )

  obj <- meta_date(varname = "date")

  obj <- meta_datetime(varname = "var1")

  obj <- meta_graph(varname = "var1", idvarname = "idvar", direction = "to")

  obj <- meta_geo(varname = "var1", latvar = "lat", longvar = "long")

  obj <- meta_href(varname = "var1")
})
