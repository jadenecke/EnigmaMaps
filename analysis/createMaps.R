library(fsbrain)
library(data.table)
library(stringr)

#read in summary statistic files:
sFiles <- list.files("ENIGMA/enigmatoolbox/datasets/summary_statistics/", pattern = "*.csv", full.names = TRUE)
summaryStats <- lapply(sFiles, fread)
names(summaryStats) <- basename(sFiles)

#define plot aggregates:
mapTypesRegex <- c("CorticalThickness", "SurfaceArea", "SubcorticalVolume|SuborticalVolume")

typeVec <- rep(NA, length(summaryStats))
for(type in mapTypesRegex){
  for(i in seq_along(summaryStats)){
    if (any(str_detect(names(summaryStats[[i]]), type))){
      typeVec[[i]] <- type
    }
  }
}

# make one giga map per map Type: