library(ggplot2)
library(vegan)

# import feature table
ftt <- read.csv("Fig6a_input.csv", header = TRUE, row.names=1, sep = ",")

# delete redundant columns
ftt <- t((ftt[rowSums(ftt[, -1]) != 0, ]))

# calculate distance matrix
require(vegan)
dm <- vegdist(ftt, method="bray")
dm <- as.matrix(dm)

# import metadata as variable
meta <- read.csv("meta.csv", header = TRUE, row.names=1, sep = ",")
time <- as.factor(meta[, 7])
host <- as.factor(meta[, 8])
DistBoxplot(dm, group = time, group_name="AMPM_", dm_name="BC", IndividualID = host)
