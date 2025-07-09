library(reshape2)
library(ggplot2)
library(RColorBrewer)

result <- read.table("fig2a_input.txt", sep = "\t", header = T)
data <- melt(result, id.vars="SampleID")
data <- data[order(data$variable), ]

nc <- length(unique(data$variable))
print(nc)

colors = c("ghostwhite", brewer.pal(3, "Set3"), 
           brewer.pal(8, "Pastel2"), 
           brewer.pal(8, "Pastel1"))

plot <- ggplot(data = data, aes(SampleID, value, fill = variable)) +
  scale_fill_manual(values = colors, guide = guide_legend(ncol = 1, bycol = TRUE)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.8, linewidth = 0.1) +
  labs(x = "", y = "Relative Abundance", fill = "") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.25),
    axis.line = element_line(linewidth = 0.25),
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20, angle = 90, vjust = 0.25),
    legend.text = element_text(size = 20),
    legend.position = "right"
  )

print(plot)
