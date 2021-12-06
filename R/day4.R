process_data_day4 <- function(input) {
  called_numbers <- readr::read_lines(input, n_max = 1) %>%
    stringr::str_split(",") %>%
    unlist()

  grids <- readr::read_lines(input, skip = 1, skip_empty_rows = TRUE)

  grid_list <- list()

  for (grid in seq_len(length(grids) / 5)) {
    grid_list[[grid]] <- grids[((5 * grid) - 4):(5 * grid)]
  }

  grid_list <- purrr::map(grid_list,
                          ~stringr::str_squish(.x) %>%
               stringr::str_split(" +?", simplify = TRUE)

  )

  return(list(called_numbers = called_numbers,
              grids = grid_list))
}

numbers <- process_data_day4(aoc_get_data(4))[[1]]
grids <- process_data_day4(aoc_get_data(4))[[2]]

for (n in 25:100) {
  current_nums <- numbers[1:n]

  rows <- purrr::map(grids,
                     ~matrix(ifelse(.x %in% current_nums, 1, 0), ncol = 5) %>%
                       rowSums())

  cols <- purrr::map(grids,
                     ~matrix(ifelse(.x %in% current_nums, 1, 0), ncol = 5) %>%
                       colSums())

  winners <- purrr::map2(rows, cols, ~ any(.x ==5) | any(.y == 5))

  n_winners <- winners %>%
    unlist() %>%
    sum()

  if (n_winners == 1) {
    winner <- which(unlist(winners))
    break()
  }
}

sum_unmarkerd <- grids[[winner]][which(!(grids[[winner]] %in% current_nums))] %>%
  as.integer() %>%
  sum()

last_called <- rev(current_nums)[1] %>%
  as.integer()
# Part 1 answer
sum_unmarkerd * last_called


# Part 2 ------------------------------------------------------------------


for (n in 25:100) {
  current_nums <- numbers[1:n]

  rows <- purrr::map(grids,
                     ~matrix(ifelse(.x %in% current_nums, 1, 0), ncol = 5) %>%
                       rowSums())

  cols <- purrr::map(grids,
                     ~matrix(ifelse(.x %in% current_nums, 1, 0), ncol = 5) %>%
                       colSums())

  winners <- purrr::map2(rows, cols, ~ any(.x ==5) | any(.y == 5))

  n_winners <- winners %>%
    unlist() %>%
    sum()

  if (n_winners == length(grids) - 1) {
    loser <- which(!unlist(winners))
  } else if (n_winners == length(grids)) {
    break()
  }
}

sum_unmarkerd <- grids[[loser]][which(!(grids[[loser]] %in% current_nums))] %>%
  as.integer() %>%
  sum()


last_called <- rev(current_nums)[1] %>%
  as.integer()

sum_unmarkerd * last_called

