test_that2("view class", {
  x <- View$new(
    name = "test view",
    state_labels(c("manufacturer", "class")),
    state_sort("manufacturer")
  )

  expect_equal(
    as.character(x$as_json(pretty = FALSE)),
    '{"name":"test view","state":{"layout":null,"labels":{"varnames":["manufacturer","class"],"type":"labels"},"sort":[{"metatype":null,"dir":"asc","varname":"manufacturer","type":"sort"}],"filter":[],"filterView":[]}}'
  )

  x <- View$new(name = "test view")
  expect_equal(
    as.character(x$as_json(pretty = FALSE)),
    '{"name":"test view","state":{"layout":null,"labels":null,"sort":[],"filter":[],"filterView":[]}}'
  )

  expect_equal(
    as.character(x$get("state")$as_json(pretty = FALSE)),
    '{"layout":null,"labels":null,"sort":[],"filter":[],"filterView":[]}'
  )
})
