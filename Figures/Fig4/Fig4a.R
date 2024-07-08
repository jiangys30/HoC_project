library(ggplot2)
library(dplyr)

data <- read.table("alpha-diversity.tsv", header = TRUE, sep = "\t")

boxplot <- ggplot(data, aes(x = time, y = shannon_entropy, fill = time)) +
  geom_boxplot(color = "black",
               fill = c("#7CCD7C", "#87CEEB"),
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

wilcox_result <- wilcox.test(shannon_entropy ~ time, data = data)
print(wilcox_result)

ggsave("shannon_withn.pdf", plot = boxplot, width = 8, height = 6, device = "pdf")
