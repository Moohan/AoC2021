library(tidyverse)

read_lines(aoc_get_data(5))[1:10] %>%
  str_match("^(\\d+?),(\\d+?) -> (\\d+?),(\\d+?)$") %>%
  .[,2:5] %>%
  as_tibble(.name_repair = ~c("x1", "y1", "x2", "y2")) %>%
  # horizontal / vertical only
  filter(x1 == x2 | y1 == y2) %>%
  pivot_longer(cols = c(x1, x2),
                names_to = c("x", "num"),
                names_pattern = "(\\w)(\\d)")

tibble(x = c(1,2,3,4,5), y = c(2,4,5,5,1)) %>%
  ggplot(aes(x,y)) +
  geom_point()
