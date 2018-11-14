suppressWarnings(library(devtools))
suppressWarnings(library(readr))
suppressWarnings(library(dplyr))
suppressWarnings(library(forcats))


context("test-axis_table")

test_that("axis_table on y axis without errors", {
  ORs <- tibble(
    Name = as.factor(paste("Group", LETTERS[1:26])),
    `Odds Ratio` = 10^rnorm(n = 26, sd = 0.1),
    Upper = 10^(log10(`Odds Ratio`) + runif(26, min = 0.1)),
    Lower = 10^(log10(`Odds Ratio`) - runif(26, min = 0.1))
  ) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(Name = fct_rev(Name))


  fig=ggplot(ORs) +
    aes(y = Name, yend = Name) +
    geom_point(aes(x = `Odds Ratio`)) +
    geom_segment(aes(x = Lower, xend = Upper)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    ylab("") +
    xlab("Odds Ratio") +
    axis_table(ORs, y = "Name", selected_cols = c("Name", "Odds Ratio")) +
    theme_classic() +
    theme(
      axis.text.y = element_text(hjust = 0, family = "mono", color = "black"),
      axis.ticks.y = element_blank()
    ) +
    scale_x_log10()
    expect_true(is.ggplot(fig))
})


test_that("axis_table on x axis without errors", {
  ORs <- tibble(
    Name = as.factor(paste("Group", LETTERS[1:26])),
    `Odds Ratio` = 10^rnorm(n = 26, sd = 0.1),
    Upper = 10^(log10(`Odds Ratio`) + runif(26, min = 0.1)),
    Lower = 10^(log10(`Odds Ratio`) - runif(26, min = 0.1))
  ) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate(Name = fct_rev(Name))


  fig=ggplot(ORs) +
    aes(x = Name, xend = Name) +
    geom_point(aes(y = "Odds Ratio")) +
    geom_segment(aes(y = Lower, yend = Upper)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    xlab("") +
    ylab("Odds Ratio") +
    axis_table(ORs, x = "Name", selected_cols = c("Name", "Odds Ratio")) +
    theme(
      axis.text.x = element_text(family = "mono", color = "black", angle = -90, vjust = 0),
      axis.ticks.x = element_blank()
    ) +
    scale_y_log10()
    expect_true(is.ggplot(fig))
})
