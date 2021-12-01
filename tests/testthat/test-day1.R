example_input <- c(
  199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263
)

test_that("part 1 example works", {
  flagged_data <- flag_increase(example_input)

  expect_equal(flagged_data$increase, c(0, 1, 1, 1, 0, 1, 1, 1, 0, 1))

  expect_equal(count_increases(flagged_data), 7)
})


test_that("part 2 example works", {
  expect_equal(rolling_sum(example_input, 2), c(607, 618, 618, 617, 647, 716, 769, 792))

  flagged_data <- flag_increase(rolling_sum(example_input, 2))

  expect_equal(flagged_data$increase, c(0, 1, 0, 0, 1, 1, 1, 1))

  expect_equal(count_increases(flagged_data), 5)
})
