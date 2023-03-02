# Load necessary R packages
library(reshape2)    # Used for data restructuring
library(ggplot2)     # Used for data visualization
library(RColorBrewer)    # Used for color palette

# Read data from a file
result <- read.table("2b16s_bench_genus.txt", sep = "\t", header = T)

# Convert data to long format for plotting purposes
data <- melt(result, id.vars="SampleID")

# Sort data by variable (genus) name
data <- data[order(data$variable), ]

# Check how many unique variables (genera) are present to allocate colors
nc <- length(unique(data$variable))
print(nc)

# Define a color palette for different genera to be plotted, with ghostwhite assigned to "false positive signal" in the first column of imported data
colors = c("ghostwhite", brewer.pal(3, "Set3"), 
           brewer.pal(8, "Pastel2"), 
           brewer.pal(8, "Pastel1"))

# Create a ggplot object, specifying x-axis, y-axis, and fill color by genus
plot <- ggplot(data=data,aes(SampleID,value,fill=variable)) +
  scale_fill_manual(values = colors) +  # Fill genera with custom colors
  geom_bar(stat="identity",position="stack", color="black", width=0.5,linewidth=0.1)+  # Draw stacked bar plot, with each bar representing a sample and relative abundance of each genus represented by bar height
  coord_flip() +  # Flip x-axis and y-axis
  labs(x = "",y = "Relative Abundance")+  # Set x-axis and y-axis labels
  scale_y_continuous(expand = c(0,0))+  # Set y-axis range
  theme_classic()+  # Set graph theme, including fill, axis lines, axis labels, legend, etc.
  theme(panel.background=element_rect(fill="white",colour="black",linewidth=0.25),
        axis.line=element_line(colour="black",linewidth=0.25),
        axis.title=element_text(size=16,color="black"),
        axis.text=element_text(size=20,color="black"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.position="bottom", legend.box = "horizontal")

# Display the plotted graph
plot(plot)

# Save the graph as a PDF file
ggsave(filename="genus_bar_plot.pdf", plot=plot, width=19, height=15)
