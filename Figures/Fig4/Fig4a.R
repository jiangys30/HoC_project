library(ggplot2)
library(dplyr)

data <- read.table("alpha_2b.tsv", header = TRUE, sep = "\t")

boxplot <- ggplot(data, aes(x = periodontal_health, y = shannon_entropy, fill = periodontal_health)) +
  geom_boxplot(color = "black",
               fill = c("#F4B0AB", "#B0C9DE"),
               linetype = "solid",
               size = 0.3) +
  labs(x = "", y = "Shannon Entropy") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

boxplot <- boxplot + scale_x_discrete(labels = function(x) sapply(strwrap(x, width = 15, simplify = FALSE), paste, collapse = "\n"))

print(boxplot)

wilcox_result <- wilcox.test(shannon_entropy ~ periodontal_health, data = data)
print(wilcox_result)
