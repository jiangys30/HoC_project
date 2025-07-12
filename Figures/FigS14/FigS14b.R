library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("FigS14b_input.csv", header = TRUE)

data$time <- factor(data$time, levels = c("9AM", "5PM"))

ggplot(data, aes(x = time, y = relative_abd)) +
  geom_boxplot(aes(group = time), fill = "white", color = "black", width = 0.5) +
  geom_line(aes(group = host, color = host), linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = host), size = 3) +
  scale_color_brewer(palette = "Pastel1") +
  facet_wrap(~ type, ncol = 2) +
  labs(title = "Comparison of P.endodontalis' abundance\nshifting trends between methods",
       x = "Time",
       y = "Relative Abundance",
       color = "Host") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    strip.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "lightgrey")
  )

# Statistic analysis
wide_data <- data %>%
  pivot_wider(
    id_cols = c(host, type),
    names_from = time,
    values_from = relative_abd
  )

perform_t_test <- function(df) {
  t.test(df$`9AM`, df$`5PM`, paired = TRUE)
}

t_test_results <- wide_data %>%
  group_by(type) %>%
  summarise(
    t_value = round(perform_t_test(cur_data())$statistic, 4),
    df = perform_t_test(cur_data())$parameter,
    p_value = format.pval(perform_t_test(cur_data())$p.value, digits = 4),
    mean_diff = round(perform_t_test(cur_data())$estimate, 6),
    ci_low = round(perform_t_test(cur_data())$conf.int[1], 6),
    ci_high = round(perform_t_test(cur_data())$conf.int[2], 6)
  )
print(t_test_results)
