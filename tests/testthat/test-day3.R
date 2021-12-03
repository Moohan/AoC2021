test_that("day 3 part 1 works", {

  # Binary Mode
  expect_equal(binary_mode(c(0, 1, 0, 1, 1)), 1)
  expect_equal(binary_mode(c(0, 1, 0, 1, 0)), 0)
  # Needed for part 2
  expect_equal(binary_mode(c(0, 1, 0, 1, 1, 0)), 1)

  example_input <- c(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  gamma_byte <- gamma_byte(process_input_day3(example_input))
  gamma_rate <- bin_to_dec(gamma_byte)

  epsilon_byte <- epsilon_byte(process_input_day3(example_input))
  epsilon_rate <- bin_to_dec(epsilon_byte)
  # Gamma byte
  expect_equal(gamma_byte, c(1, 0, 1, 1, 0))

  # Gamma rate
  expect_equal(gamma_rate, 22L)

  # Epsilon rate
  expect_equal(epsilon_rate, 9L)

  # Answer
  expect_equal(day3_part1_answer(example_input), 198L)
})

test_that("day 3 part 2 works", {

  # Binary Mode - part 2
  expect_equal(binary_mode(c(0, 1, 0, 1, 1, 0)), 1)

  example_input <- c(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  oxygen_byte <- oxygen_byte(process_input_day3(example_input))
  oxygen_rate <- bin_to_dec(oxygen_byte)

  co2_byte <- co2_byte(process_input_day3(example_input))
  co2_rate <- bin_to_dec(co2_byte)

  # O2 byte
  expect_equal(oxygen_byte, c(1, 0, 1, 1, 1))

  # 02 rate
  expect_equal(oxygen_rate, 23L)

  # C02 byte
  expect_equal(co2_byte, c(0, 1, 0, 1, 0))

  # C02 rate
  expect_equal(co2_rate, 10L)

  # Answer
  expect_equal(day3_part2_answer(example_input), 230L)
})
