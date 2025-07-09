library(datasets)
library(tidyverse)

base_name <- "2b16S"

input_file <- paste0("combined_data_", base_name, ".csv")
output_file <- paste0("combined_data_", base_name, ".pdf")
correlation_file <- paste0("correlation_stat_", base_name, ".csv")

combined_data <- read.csv(input_file, header = TRUE, sep = "," )

correlation_data <- read.csv(correlation_file, header = TRUE, sep = ",")

combined_data <- combined_data %>%
  left_join(correlation_data, by = c("Group" = "id"))

label_data <- combined_data %>%
  distinct(Group, .keep_all = TRUE) %>%
  mutate(
    label = sprintf("R = %.2f\nL2 = %.2f", R, L2),
    x = -4.5,   
    y = -1    
  )

color_mapping <- c("shared" = "#87CEFA", "2bRAD-M only" = "#00FF7F", "(WMS) 2bRAD-M only" = "#9370DB", 
                   "(WMS) MPA only" = "#FFD700", "16S only" = "#FFC0CB")

plot <- ggplot(combined_data,aes(x=(log10(col_2b)),y=(log10(col_16S)),color=Color))+
  theme_bw()+
  geom_point(size=6,alpha=0.5)+
  geom_text(
    data = label_data,
    aes(x = x, y = y, label = label),
    color = "black", size = 6, hjust = 0, vjust = 0
  ) +
  theme(legend.position="none",
        plot.title = element_text(face="bold", size=18,hjust=0.5),
        axis.text.y=element_text(size=16,angle=90,hjust=0.5),
        axis.text.x=element_text(size=16),
        axis.title=element_text(size=16),
        strip.text.x=element_text(size=16),
        panel.grid = element_blank(),  
        legend.key.size = unit(2, "lines"),  
        legend.text = element_text(size=16), 
        legend.title = element_blank())+  
  geom_smooth(method="lm", level = 0.97,)+
  scale_x_continuous(limits=c(-4.5,0), breaks=c(-4,-3,-2,-1,0),
                     labels=c("0.01%","0.1%","1%","10%","100%"))+
  scale_y_continuous(limits=c(-4.5,0), breaks=c(-4,-3,-2,-1,0),
                     labels=c("0.01%","0.1%","1%","10%","100%"))+
  labs(title = "2bRAD-M vs. 16S at species resolution",
       y='16S relative abundance',x='2bRAD-M relative abundance')+
  scale_color_manual(values = color_mapping) +  
  facet_wrap(~Group,scales="fixed", nrow = 1) 
print(plot)
