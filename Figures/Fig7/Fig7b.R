meta <- read.table("Fig7b_input_2b_metadata.txt", sep = "\t", header = T, row.names = 1)

pcs <- read.table("2Fig7b_input_2b_pcs_BC.txt", header = TRUE, sep = "\t", row.names = 1)
colnames(pcs) <- c("PC1", "PC2")

pcs <- pcs[rownames(meta), ]

print(identical(rownames(pcs), rownames(meta)))

data <- merge(pcs, meta["status"], by = 0, all = T)

library(ggplot2)

plot <- ggplot(data,aes(x = PC1, y = PC2, color = status, Group = status)) +
  geom_point(size=3)+
  theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.ticks = element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        legend.text = element_text(size=14, color="black"),
        legend.key.size = unit(0.8,"cm"),
        legend.title = element_blank(),
        plot.margin = unit(rep(1.5,4),'lines')) +
  ggtitle("SHIH (PCoA based on Weighted UniFrac Distance)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5))  # Adjust title size and alignment
print(plot)
