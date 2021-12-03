test_that("part 2 works", {
  example_input <- c(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  final_position <- c(15, 60)
  answer <- 900

  expect_equal(
    apply_aim_moves(position = c(0, 0, 0), input = example_input),
    final_position
  )

  expect_equal(
    part_2_answer(position = c(0, 0, 0), input = example_input),
    answer
  )
})
