# Load required R packages
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# Read in data from file
result <- read.table("2b16s_bench_species.txt", sep = "\t", header = T)

# Convert data to long format for plotting
data <- melt(result, id.vars="SampleID")

# Sort the data
data <- data[order(data$variable), ]

# Check how many unique genus there are for assigning colors
nc <- length(unique(data$variable))
print(nc)

# Define a set of colors to use for plotting different genus, note that "false positive signals" should be in the first column of imported data and assigned color ghostwhite
colors = c("ghostwhite", brewer.pal(4, "Set3"), 
           brewer.pal(8, "Pastel2"), 
           brewer.pal(8, "Pastel1"))

# Create a ggplot object and specify x-axis, y-axis, and fill color by genus
plot <- ggplot(data=data,aes(SampleID,value,fill=variable)) +
  scale_fill_manual(values = colors) +  # Use custom colors to fill genus
  # Draw a stacked bar plot, where each bar represents a sample, and the relative abundance of each genus is represented by the height of each stack
  geom_bar(stat="identity",position="stack", color="black", width=0.5,size=0.1)+
  coord_flip() +  # Swap x-axis and y-axis
  labs(x = "",y = "Relative Abundance")+  # Set x-axis and y-axis labels
  scale_y_continuous(expand = c(0,0))+  # Set y-axis range
  theme_classic()+
  # Set plot theme including fill box, axis line, axis label, legend, etc.
  theme(panel.background=element_rect(fill="white",colour="black",linewidth=0.25), # Fill box theme color, border color, and border line thickness
        axis.line=element_line(colour="black",linewidth=0.25), # x and y axis color and thickness
        axis.title=element_text(size=16,color="black"), # Set x and y axis label
        axis.text=element_text(size=20,color="black"), # Set x and y axis text
        legend.title=element_text(size=20),
        legend.text=element_text(size=16),
        legend.position="bottom", legend.box = "horizontal")

# Display the plotted figure
plot(plot)

# Save the figure as a PDF file
ggsave(filename="species_bar_plot.pdf", plot=plot, width=19, height=15)
