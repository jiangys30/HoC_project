library(ggplot2)

data <- read.csv("BC_all.csv", header = TRUE)

plot <- ggplot(data, aes(x = depth, y = BrayCurtis_similarity, color = sample_id)) +
  geom_line(linewidth = 1) +
  xlab("Subsampling depth (million reads / sample)") +
  ylab("Bray-Curtis dissimilarity") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.key.size = unit(1, "lines")
  ) +
  facet_wrap(~ way, nrow = 1) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

print(plot)

ggsave("new_BC_with_same_scale.pdf", plot = plot, width = 10, height = 3)


data_2b <- read.csv("BC_2b.csv", header = TRUE)

plot_2b <- ggplot(data_2b, aes(x = depth, y = BrayCurtis_similarity, color = sample_id)) +
  geom_line(linewidth = 1) +
  xlab("") +
  ylab("Bray-Curtis dissimilarity") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

print(plot_2b)

ggsave("BC_2b.pdf", plot = plot_2b, width = 5, height = 2.5)
