forward <- function(position, n) {
  position[1] <- position[1] + n

  return(position)
}

up <- function(position, n) {
  position[2] <- position[2] - n

  return(position)
}

down <- function(position, n) {
  position[2] <- position[2] + n

  return(position)
}

move <- function(position, direction, n) {
  switch(direction,
    "forward" = forward(position, n),
    "up" = up(position, n),
    "down" = down(position, n)
  )
}

process_input_day2 <- function(lines) {
  split_list <- stringr::str_split(lines, pattern = "\\s")

  split_list <- purrr::map(split_list, ~ list(.x[1], as.integer(.x[2])))

  return(split_list)
}

apply_moves <- function(position, input) {
  moves <- purrr::map(
    process_input_day2(input),
    ~ move(position, .x[[1]], .x[[2]])
  )

  h_pos <- sum(purrr::map_dbl(moves, ~ .x[1]))
  depth <- sum(purrr::map_dbl(moves, ~ .x[2]))

  return(c(h_pos, depth))
}

part_1_answer <- function(position, input) {
  new_position <- apply_moves(position, input)

  answer <- new_position[1] * new_position[2]

  return(answer)
}
