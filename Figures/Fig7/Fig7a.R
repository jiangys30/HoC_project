library(ggplot2)
library(dplyr)

data <- read.table("Fig7a_input_2b.tsv", header = TRUE, sep = "\t")

boxplot <- ggplot(data, aes(x = Health_status, y = shannon_entropy, fill = Health_status)) +
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

wilcox_result <- wilcox.test(shannon_entropy ~ Health_status, data = data)
print(wilcox_result)
