library(tidyverse)
library(mediocrethemes)
library(plotly)

set_mediocre_all(pal = "portal")

n <- 1000
alpha <- 15000
beta <- 2000
sigma_u <- 8000

dat <- 
  tibble(
    educ = rnorm(n, 3, 1),
    e = rnorm(n, 0, sigma_u),
    wage = alpha + beta * educ + e
  ) 

dat_sample <- dat |> 
  mutate(
    in_sample = rbernoulli(n, p = 0.1),
    in_sample_name = ifelse(in_sample, "Within Sample",NA)
  ) 

dat_sample |> 
  # mutate(in_sample = ifelse(!in_sample, NA, in_sample)) |>
  ggplot(aes(x = educ, y = wage, color = in_sample_name)) + 
  geom_point(size = 3) +
  geom_abline(intercept = alpha, slope = beta) +
  geom_smooth(data = dat_sample |> filter(in_sample), method = "lm", se = FALSE, fullrange = TRUE) +
  labs(
    color = NULL
  )
