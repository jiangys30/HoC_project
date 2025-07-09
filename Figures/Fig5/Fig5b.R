library(tidyr)
library(ggplot2)
library(dplyr)

data <- read.csv("tumor_L2.csv")

colnames(data) <- gsub("^X", "", colnames(data))
colnames(data) <- gsub("^2b", "2B", colnames(data))

data_long <- data %>%
  pivot_longer(
    cols = -id,
    names_to = "Group",
    values_to = "R_value"
  ) %>%
  mutate(Group = factor(Group, levels = colnames(data)[-1]))

ggplot(data_long, aes(x = Group, y = R_value, fill = Group)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 3, alpha = 0.8, shape = 16) +
  labs(title = "Comparison of L2 Similarity Scores",
       x = "Comparison Groups (n=4 per group)",
       y = "L2 Similarity Score") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  ylim(0, 1)

perform_pairwise_tests <- function(data) {
  groups <- colnames(data)[-1]
  combn(groups, 2, simplify = FALSE) %>%
    map_df(function(pair) {
      t_test <- t.test(data[[pair[1]]], data[[pair[2]]], paired = TRUE)
      wilcox_test <- wilcox.test(data[[pair[1]]], data[[pair[2]]], paired = TRUE)
      
      data.frame(
        Comparison = paste(pair[1], "vs", pair[2]),
        t_statistic = t_test$statistic,
        t_p_value = t_test$p.value,
        wilcox_statistic = wilcox_test$statistic,
        wilcox_p_value = wilcox_test$p.value
      )
    }) %>%
    mutate(
      t_adj_p = p.adjust(t_p_value, method = "BH"),
      wilcox_adj_p = p.adjust(wilcox_p_value, method = "BH")
    )
}

test_results <- perform_pairwise_tests(data)
print(test_results, digits = 4)
