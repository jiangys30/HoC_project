library(ggplot2)
merge2 <- read.csv("genus_16S_2b.csv",header = TRUE, sep = "," )
color_mapping <- c("shared" = "#87CEFA", "16S only" = "#FFC0CB", "2b only" = "#00FF7F")
plot <- ggplot(merge2, aes(x = log10(col_2b), y = log10(col_16S), color = Color)) +
  theme_bw() +
  geom_point(size = 6, alpha = 0.5) +
  theme(
    legend.position = "bottom", 
    axis.text.y = element_text(size = rel(2), angle = 90, hjust = 0.5),
    axis.text.x = element_text(size = rel(2)),
    axis.title = element_text(size = 30),
    strip.text.x = element_text(size = rel(2)),
    panel.grid = element_blank(),
    legend.key.size = unit(2, "lines"),
    legend.text = element_text(size = 20),
    legend.title = element_blank()
  ) +
  geom_smooth(method = "lm", level = 0.97) +
  scale_x_continuous(limits = c(-4.5, 0), breaks = c(-4, -3, -2, -1, 0),
                     labels = c("0.01%", "0.1%", "1%", "10%", "100%")) +
  scale_y_continuous(limits = c(-4.5, 0), breaks = c(-4, -3, -2, -1, 0),
                     labels = c("0.01%", "0.1%", "1%", "10%", "100%")) +
  labs(x = '2bRAD-M relative abundance', y = '16S relative abundance') +
  scale_color_manual(values = color_mapping) +
  facet_wrap(~Group, scales = "fixed", nrow = 4)
print(plot)
