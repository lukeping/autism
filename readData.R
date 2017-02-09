
require(dplyr)
require(readr)
require(gridExtra)
require(grid)
require(ggplot2)
require(e1071)
require(ROCR)
require(VennDiagram)
require(openxlsx)

source("/Users/wangys/data/autism/data/aut.R")
source("/Users/wangys/data/autism/data/msvmRFE.R")

# Read metabolon data
d1 <- read_tsv("/Users/wangys/data/autism/data/d1.txt", col_types = list(COMPID = "c"))
d2 <- read_tsv("/Users/wangys/data/autism/data/d2.txt", col_types = list(COMPID = "c"))
d3 <- read_tsv("/Users/wangys/data/autism/data/d3.txt", col_types = list(COMPID = "c"))
d1[,-1] <- log2(d1[,-1])
d2[,-1] <- log2(d2[,-1])
d3[,-1] <- log2(d3[,-1])
chem <- read_tsv("/Users/wangys/data/autism/data/chem.txt", col_types = ("ccc"))
row.names(chem) <- chem$COMPID
samples <- read_tsv("/Users/wangys/data/autism/data/samples.txt", col_types = "cdccccccd")
samples$AGEGROUP <- NA
samples$AGEGROUP[which(samples$AGE <= 5)] <- "1"
samples$AGEGROUP[which(samples$AGE <= 10 & samples$AGE > 5)] <- "2"
samples$AGEGROUP[which(samples$AGE <= 15 & samples$AGE > 10)] <- "3"
samples$AGEGROUP[which(samples$AGE > 15)] <- "4"
samples$TYPE <- samples$GROUP
samples$TYPE[which(samples$GROUP == "ASD" | samples$GROUP == "NDD")] <- "DD"
samples <- as.data.frame(samples)
samples$GROUP <- factor(samples$GROUP)
row.names(samples) <- samples$ID
