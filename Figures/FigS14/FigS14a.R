library(ggplot2)
library(ggpubr)
library(readr)

data <- read_csv("FigS14a_input.csv")

ggplot(data, aes(x = `2bRAD-M`, y = qPCR)) +
  geom_point(size = 3, shape = 21, fill = "steelblue", color = "white", stroke = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "red2", alpha = 0.2) +
  stat_cor(method = "pearson", 
           label.x.npc = "left", 
           label.y.npc = "top", 
           size = 6) +
  labs(
    title = "Correlation between 2bRAD-M and qPCR\nMeasurements on P.endodontalis' relative abundance",
    x = "2bRAD-M relative abundance",
    y = "qPCR-derived relative abundance"
  ) +
  scale_x_continuous(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0.05, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    panel.grid.minor = element_blank(),
    aspect.ratio = 1
  )
