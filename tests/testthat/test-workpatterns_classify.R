test_that(
  desc = "Test workpatterns outputs",
  code = {
    out <- workpatterns_classify(em_data, signals = "IM", return = "data")
    expect_equal(
      object = nrow(out),
      expected = nrow(em_data)
    )
  }
)
