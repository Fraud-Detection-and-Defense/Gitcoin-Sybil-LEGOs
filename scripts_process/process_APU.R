library(jsonlite)
library(readr)

## Read in APU data
apudf <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/stamps_and_scores.csv")
final <- data.frame(Handle = apudf$handle,Score = apudf$last_apu_score)

## Write out final file
write_csv(final,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/APU.csv")


## Stamps Wide format
# apudfs <- lapply(apudf$stamps,fromJSON)
# all_stamps <- unique(unlist(apudfs))
# stamps_wide <- cbind(Handle = apudf$handle, as.data.frame(do.call(rbind,lapply(apudfs,function(x,all_stamps) all_stamps %in% x,all_stamps=all_stamps))))
# names(stamps_wide)[-1] <- all_stamps
# write_csv(stamps_wide,"~/Downloads/stamps_wide.csv")