#
#
#
#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
#| cache: true
throw_dice <- function(n, weights = rep(1 / 6, 6)) {
  die1 <- sample(1:6, size = n, replace = TRUE, prob = weights)
  die2 <- sample(1:6, size = n, replace = TRUE, prob = weights)
  die1 + die2
}
#
#
#
set.seed(10)

n_values <- c(10, 20, 50, 100, 1000)

samples <- map(n_values, ~ throw_dice(.x))

df <- tibble(
  n = factor(rep(n_values, times = n_values), levels = n_values),
  sum = unlist(samples)
)

ggplot(df, aes(sum)) +
  geom_histogram(binwidth = 1, color = "white") +
  facet_wrap(~ n, scales = "free_y") +
  scale_x_continuous(breaks = 2:12)
#
#
#
#
