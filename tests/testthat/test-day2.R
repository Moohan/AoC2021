example_input <- c("forward 5",
                    "down 5",
                    "forward 8",
                    "up 3",
                    "down 8",
                    "forward 2")


test_that("part 1 works", {
  final_postition <- c(15, 10)
  answer <- 150

  expect_equal(apply_moves(position = c(0, 0), input = example_input), final_postition)

  expect_equal(part_1_answer(position = c(0, 0), input = example_input), answer)
})
