test_that(
  desc = "workpatterns data output row matches - signals",
  code = {
    out <- workpatterns_classify(em_data, signals = "IM", return = "data")
    expect_equal(
      object = nrow(out),
      expected = nrow(em_data)
    )

    out <- workpatterns_classify(em_data, signals = "unscheduled_calls", return = "data")
    expect_equal(
      object = nrow(out),
      expected = nrow(em_data)
    )

    out <- workpatterns_classify(em_data, signals = "meetings", return = "data")
    expect_equal(
      object = nrow(out),
      expected = nrow(em_data)
    )
  }
)

test_that("workpatterns plots returns ggplot object",{
  p <- workpatterns_classify(em_data, signals = "meetings", return = "plot")
  expect_s3_class(p, "ggplot")

  p <- workpatterns_classify(em_data, signals = "IM", return = "plot")
  expect_s3_class(p, "ggplot")

  p <- workpatterns_classify(em_data, signals = "unscheduled_calls", return = "plot")
  expect_s3_class(p, "ggplot")
})
