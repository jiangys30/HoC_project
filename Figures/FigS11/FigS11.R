library(ggplot2)
library(dplyr)

data <- read.csv("2bRADM_microbial_3bins.csv") %>%
  mutate(depth_factor = factor(depth, levels = sort(unique(depth))))

median_data <- data %>%
  group_by(depth_factor) %>%
  summarise(median_value = median(microbial_reads, na.rm = TRUE))

ggplot(data, aes(x = depth_factor, y = microbial_reads)) +
  geom_boxplot(
    width = 0.6,
    outlier.size = 1,
    color = "black",
    fill = "white"
  ) +
  geom_line(
    data = median_data,
    aes(x = depth_factor, y = median_value, group = 1),
    color = "gray40",
    size = 1,
    lineend = "round"
  ) +
  stat_summary(
    fun = median,
    geom = "errorbar",
    aes(ymax = after_stat(y), ymin = after_stat(y)),
    color = "#87CEFA",
    width = 0.6,
    size = 1.5
  ) +
  scale_y_continuous(
    trans = "log10", 
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  geom_hline(
    yintercept = c(1e3, 1e4, 1e5, 1e6, 1e7), 
    linetype = "dashed", 
    color = "gray75", 
    alpha = 0.5
  ) +
  labs(
    x = "Subsampling depth (million reads)",
    y = "Microbial reads count (log scale)"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20, margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, margin = margin(r = 15)),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 20)
  )
