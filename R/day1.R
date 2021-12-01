#' Flag increases
#'
#' @param depth_input depth input
#'
#' @return
#' @export
flag_increase <- function(depth_input) {
  data <- tibble::tibble(depth = depth_input)

  data %>%
    dplyr::mutate(
      increase = depth > dplyr::lag(depth),
      increase = tidyr::replace_na(increase, 0)
    )
}


#' Count number of increases
#'
#' @param data data with depth and increase flagged
#'
#' @return
#' @export
count_increases <- function(data) {
  data %>%
    dplyr::mutate(n_increase = cumsum(increase)) %>%
    dplyr::pull(n_increase) %>%
    max()
}

# Part 1
readr::read_lines(aoc_get_data(1)) %>%
  readr::parse_integer() %>%
  flag_increase() %>%
  count_increases()


# Part 2

#' Create rolling sum
#'
#' @param depth_input data
#' @param n number to roll over
#'
#' @return vector of integers
#' @export
rolling_sum <- function(depth_input, n) {
  roll_sum <- slider::slide_int(depth_input, ~ sum(.x), .before = n)

  return(roll_sum[-c(1:n)])
}


readr::read_lines(aoc_get_data(1)) %>%
  readr::parse_integer() %>%
  rolling_sum(n = 2) %>%
  flag_increase() %>%
  count_increases()
