meta <- read.table("2b_meta.txt", sep = "\t", header = T, row.names = 1)
pcs <- read.table("2b_bc_pcs.txt", header = TRUE, sep = "\t", row.names = 1)
colnames(pcs) <- c("PC1", "PC2")

pcs <- pcs[rownames(meta), ]
print(identical(rownames(pcs), rownames(meta)))

data <- merge(pcs, meta["Group"], by = 0, all = T)

library(ggplot2)

plot <- ggplot(data, aes(x = PC1, y = PC2, color = Group, Group = Group)) +
  geom_point(size = 3) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size = 14, color = "black"),
    legend.key.size = unit(0.8, "cm"),
    legend.title = element_blank(),
    plot.margin = unit(rep(1.5, 4), 'lines')
  ) +
  xlab(paste0("PC1 (", round(proportion[1], 4) * 100, "%)")) +
  ylab(paste0("PC2 (", round(proportion[2], 4) * 100, "%)")) +
  ggtitle("SHIH (PCoA based on Weighted UniFrac Distance)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

print(plot)
ggsave("2bRAD_Bray_curtis.pdf", plot = plot, width = 8, height = 6)
