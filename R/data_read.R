#' Build AoC URL
#'
#' @param day
#' @param year
#'
#' @return
#' @export
aoc_build_url <- function(day, year = 2021) {
  formatted_url <- stringr::str_glue("adventofcode.com/{year}/day/{day}/input")

  return(formatted_url)
}

#' Get AoC data
#'
#' @param day
#' @param session_cookie
#' @param year
#'
#' @return
#' @export
aoc_get_data <- function(day, session_cookie = keyring::key_get("aoc"), year = 2021) {
  aoc_url <- aoc_build_url(day, year)

  day_string <- stringr::str_pad(day, width = 2, side = "left", pad = "0")

  file_path <- stringr::str_glue("data/{year}_day_{day_string}.txt")

  if (!file.exists(file_path)) {
    cookie <- httr::set_cookies(session = session_cookie)

    response <- httr::GET(aoc_url, cookie)

    httr::stop_for_status(response)

    content <- httr::content(response, as = "parsed", encoding = "UTF-8")

    readr::write_file(content, file_path)
  }

  return(file_path)
}
