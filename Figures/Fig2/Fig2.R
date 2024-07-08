library(reshape2)
library(ggplot2)
library(RColorBrewer)

result <- read.table("2b16s_bench_genus.txt", sep = "\t", header = TRUE)
data <- melt(result, id.vars="SampleID")
data <- data[order(data$variable), ]

nc <- length(unique(data$variable))
print(nc)

colors <- c("ghostwhite", brewer.pal(3, "Set3"), brewer.pal(8, "Pastel2"), brewer.pal(8, "Pastel1"))

plot <- ggplot(data, aes(SampleID, value, fill=variable)) +
  scale_fill_manual(values = colors) +
  geom_bar(stat="identity", position="stack", color="black", width=0.5, size=0.1) +
  coord_flip() +
  labs(x = "", y = "Relative Abundance") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", size = 0.25),
    axis.line = element_line(colour = "black", size = 0.25),
    axis.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 20, color = "black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

ggsave(filename = "genus_bar_plot.pdf", plot = plot, width = 19, height = 15)
