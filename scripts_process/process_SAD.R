library(jsonlite)
library(readr)

## Read in SAD data
saddf <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/gr15_contributions/20220923-003421_SAD_output.csv")
final <- data.frame(Handle = saddf$handle,Score = saddf$prediction_score)

## Write out final file
write_csv(final,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/SAD.csv")