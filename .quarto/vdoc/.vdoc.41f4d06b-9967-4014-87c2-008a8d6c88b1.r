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

weights <- c(0.25, rep(0.75 / 5, 5))
n_values <- c(10, 20, 50, 100, 1000)

samples <- map(n_values, ~ throw_dice(.x, weights = weights))

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
tibble(
  face = 1:6,
  p_loaded = c(0.30, rep(0.14, 5)),
  p_fair = rep(1 / 6, 6),
  likelihood_ratio = p_loaded / p_fair
) |>
  select(face, likelihood_ratio)
#
#
#
#
