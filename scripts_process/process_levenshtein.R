## Loading Libraries
library(readr)
library(parallel)
library(Rfast)
library(stringdist)

## Load Data
all_handles <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/handles_alltime.csv")$handle
all_handles <- all_handles[!is.na(all_handles)]

## Split and perform job in chunks
all_handles_spl <- split(all_handles, ceiling(seq_along(all_handles)/2500))

## Function to get the stats out
get_lev_stats <- function(handles,all_handles)
{
	levmat <- stringdistmatrix(handles,all_handles,method="lv")
	levmat[levmat==0] <- 100000
	levdist <- data.frame(
							Handle = handles,
							Distance_Min = rowMins(levmat,value=TRUE),
							Distance_Min_Match = all_handles[rowMins(levmat)],
							Distance_Min_is1_Count = rowsums(levmat==1),
							Distance_Min_is2_Count = rowsums(levmat==2),
							Distance_Min_is1_Matches = NA
						)
	if(sum(levdist$Distance_Min==1)>0)
	{
		levmatt <- levmat[levdist$Distance_Min==1,,drop=FALSE]
		levdist$Distance_Min_is1_Matches[levdist$Distance_Min==1] <- sapply(apply(levmatt,1,function(x) which(x==1),simplify=FALSE),function(idx,all_handles) paste0(all_handles[idx],collapse=";;;"),all_handles=all_handles)
	}
	return(levdist)
}

## Run in looped and parallel
lev_dft <- list()
for(idx in 1:length(all_handles_spl))
{
	st_time <- Sys.time()
	lev_dft[[idx]] <- get_lev_stats(all_handles_spl[[idx]],all_handles)
	en_time <- Sys.time()
	gc()
	message(paste0(idx,"/",length(all_handles_spl),":Took ",difftime(en_time,st_time,units="mins")," Minutes"))
}
lev_df <- do.call(rbind,lev_dft)

## Write out data
write_csv(lev_df,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_matches.csv")
write_csv(lev_df[,1:5],"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_scores.csv")

## Write data as fst file
library(fst)
lev_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_matches.csv")
write_fst(lev_df, "~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_matches.fst")




