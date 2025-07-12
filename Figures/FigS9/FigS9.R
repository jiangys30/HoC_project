meta <- read.table("2b_meta.txt", sep = "\t", header = T, row.names = 1)

# Read PCA data, skipping first 2 lines
pcs <- read.table("2b_bc_pcs.txt", 
                  header = FALSE, 
                  sep = "\t", 
                  skip = 2, 
                  row.names = 1)

# Set column names
colnames(pcs) <- c("PC1", "PC2")

# Reorder pcs rows to match meta row names
pcs <- pcs[rownames(meta), ]

# Verify row name alignment
print(identical(rownames(pcs), rownames(meta)))

# Merge PCA data with grouping information
data <- merge(pcs, meta["Group"], by = 0, all = TRUE)

# Create PCA plot
library(ggplot2)

plot <- ggplot(data, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 3) +
  theme_classic() +
  theme(
    axis.text = element_blank(),          # Hide axis text
    axis.ticks = element_blank(),         # Hide axis ticks
    axis.title = element_text(size = 20),  # Axis title size
    panel.grid.minor = element_blank(),    # Remove minor gridlines
    legend.text = element_text(size = 14, color = "black"),
    legend.key.size = unit(0.8, "cm"),
    legend.title = element_blank(),        # Remove legend title
    plot.margin = unit(rep(1.5, 4), "lines")
  ) +
  ggtitle("PCoA plot based on Bray-Curtis Dissimilarity") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

print(plot)
