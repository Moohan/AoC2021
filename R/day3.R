process_input_day3 <- function(input) {
  # Retunrs a matrix of integers
  stringr::str_split(input, "") %>%
    unlist() %>%
    as.integer() %>%
    matrix(nrow = length(input), byrow = TRUE)
}


binary_mode <- function(numbers) {
  mean(numbers) %>%
    janitor::round_half_up() %>%
    as.integer()
}

.most_common_bit <- function(matrix, byte) {
  binary_mode(matrix[, byte])
}
most_common_bit <- memoise::memoise(.most_common_bit)

least_common_bit <- function(matrix, byte) {
  (most_common_bit(matrix, byte) + 1) %% 2
}


gamma_byte <- function(matrix) {
  byte_length <- ncol(matrix)
  gamma_byte <- vector()

  for (i in seq_len(byte_length)) {
    gamma_byte[i] <- most_common_bit(matrix, i)
  }

  return(gamma_byte)
}

epsilon_byte <- function(matrix) {
  byte_length <- ncol(matrix)
  epsilon_byte <- vector()

  for (i in seq_len(byte_length)) {
    epsilon_byte[i] <- least_common_bit(matrix, i)
  }

  return(epsilon_byte)
}

bin_to_dec <- function(binary_vec) {
  sum(2^(which(rev(binary_vec) == 1) - 1))
}


day3_part1_answer <- function(input) {
  matrix <- process_input_day3(input)

  gamma_byte <- gamma_byte(matrix)
  epsilon_byte <- epsilon_byte(matrix)

  power_consumption <- bin_to_dec(gamma_byte) * bin_to_dec(epsilon_byte)

  return(power_consumption)
}

# Part 2

oxygen_byte <- function(matrix) {
  for (bit in seq_len(ncol(matrix))) {
    if (class(matrix) == "integer") {
      break()
    }

    matrix <- matrix[which(matrix[, bit] == most_common_bit(matrix, bit)), ]
  }

  return(matrix)
}

co2_byte <- function(matrix) {
  for (bit in seq_len(ncol(matrix))) {
    if (class(matrix) == "integer") break()

    matrix <- matrix[which(matrix[, bit] == least_common_bit(matrix, bit)), ]
  }

  return(matrix)
}


day3_part2_answer <- function(input) {
  matrix <- process_input_day3(input)

  oxygen_byte <- oxygen_byte(matrix)
  co2_byte <- co2_byte(matrix)

  life_support_rating <- bin_to_dec(oxygen_byte) * bin_to_dec(co2_byte)

  return(life_support_rating)
}
