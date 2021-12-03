# postion is c(horizontal, depth, aim)

aim_forward <- function(position, n) {
  position[1] <- position[1] + n
  position[2] <- position[2] + (position[3] * n)

  return(position)
}

aim_up <- function(position, n) {
  position[3] <- position[3] - n

  return(position)
}

aim_down <- function(position, n) {
  position[3] <- position[3] + n

  return(position)
}

aim_move <- function(position, direction, n) {
  switch(direction,
    "forward" = aim_forward(position, n),
    "up" = aim_up(position, n),
    "down" = aim_down(position, n)
  )
}


apply_aim_moves <- function(position, input) {
  new_position <- position

  input <- process_input_day2(input)

  for (i in input) {
    new_position <- aim_move(new_position, i[[1]], i[[2]])
  }

  return(new_position[1:2])
}

part_2_answer <- function(position, input) {
  new_position <- apply_aim_moves(position, input)

  answer <- new_position[1] * new_position[2]

  return(answer)
}
