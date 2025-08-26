test_that(
  desc = "flex_index data output returns data frame",
  code = {
    out <- flex_index(em_data, signals = "IM", return = "data")

    expect_s3_class(out, "data.frame")

    out <- flex_index(em_data, signals = "unscheduled_calls", return = "data")

    expect_s3_class(out, "data.frame")

    out <- flex_index(em_data, signals = "meetings", return = "data")

    expect_s3_class(out, "data.frame")
  }
)

test_that("flex_index plots returns ggplot object",{
  p <- flex_index(em_data, signals = "meetings", return = "plot")
  expect_s3_class(p, "ggplot")

  p <- flex_index(em_data, signals = "IM", return = "plot")
  expect_s3_class(p, "ggplot")

  p <- flex_index(em_data, signals = "unscheduled_calls", return = "plot")
  expect_s3_class(p, "ggplot")
})
