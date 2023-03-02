getwd()                                                                         # check working directory

sampleid <- "s13_12"                                                            # change data set here!!(write sample id)
species_2b <- read.csv(paste0("2b_", sampleid, ".csv"), header = TRUE, sep = ",")
colnames(species_2b)[1] <- "Species"
colnames(species_2b)[2] <- "col_2b"
species_WMS <- read.csv(paste0("WMS_", sampleid, ".csv"), header = TRUE, sep = ",")   # read sample data
colnames(species_WMS)[1] <- "Species"
colnames(species_WMS)[2] <- "col_WMS"
merge1 <- merge(species_2b, species_WMS, by = "Species", 
                     all.x = TRUE, all.y = TRUE)                                # merge these tables by speices, both tables should be relative abundance taxonomy files
merge1[is.na(merge1)] = 0                                                       # replace missing values of columns with 0
merge1$Group <- sampleid                                                        # add a column name "Group" into merge1, fill in the contents with the sample id
merge1$Color <- ifelse(merge1[, 2] == 0, "WMS only", 
                       ifelse(merge1[, 3] == 0, "2bRAD-M only", "shared"))      # add a column name "Color" to the right of merge1, write a judgement to assign type to the data
write.csv(merge1, paste0("samepipeline_", sampleid, ".csv"), row.names=FALSE)          # save the data frame by its sample id

cor.test(merge1$col_2b,merge1$col_WMS)                                          # calculate pearson correlation
library(vegan)
calculate <- t(merge1[,2:3])
1-vegdist(calculate, method="bray")
1-vegdist(calculate, method="euclidean")                                        # calculate BC similarity and L2 similarity
diversity(calculate,"shannon")
1-abs(1-diversity(calculate[1,],"shannon")/diversity(calculate[2,],"shannon"))  # calculate Shannon correlation

############ Concatenate all data frames and draw scatter plot #################

file_list <- list.files(pattern = "samepipeline_.*\\.csv")                             # Get all csv files prefixed with "color_" in the working directory
all_data <- do.call(rbind, lapply(file_list, read.csv, header = TRUE))          # read all files and concatenate them
write.csv(all_data, file = "samepipeline_all.csv", row.names = FALSE)                        # Write the concatenated data into a new csv file
merge2 <- read.csv("samepipeline_all.csv",header = TRUE, sep = "," )                         #import all.csv fill
library(datasets)
library(tidyverse)
merge2 <- tibble::as_tibble(merge2)
ggplot(merge2,aes(x=(log10(col_2b)),y=(log10(col_WMS)),color=Color))+
  theme_bw()+
  geom_point(size=9,alpha=0.5)+
  theme(legend.position="bottom", 
        axis.text.y=element_text(size=rel(2),angle=90,hjust=0.5),
        axis.text.x=element_text(size=rel(2)),
        axis.title=element_text(size=rel(3.5)),
        strip.text.x=element_text(size=rel(3)))+
  geom_smooth(method="lm", level = 0.97)+
  scale_x_continuous(limits=c(-4.5,0), breaks=c(-4,-3,-2,-1,0),
                     labels=c("0.01%","0.1%","1%","10%","100%"))+
  scale_y_continuous(limits=c(-4.5,0), breaks=c(-4,-3,-2,-1,0),
                     labels=c("0.01%","0.1%","1%","10%","100%"))+
  labs(x='2bRAD-M relative abundance',y='WMS relative abundance')+
  facet_wrap(~Group,scales="fixed",nrow = 4)                                    #To show legends, type: + theme(legend.position="bottom")'

ggsave("same_pipeline.pdf", width = 49, height = 26)                                      #export the .png figure
