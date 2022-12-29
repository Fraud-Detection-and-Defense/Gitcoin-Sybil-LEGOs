library(readr)
library(kit)
library(parallel)
library(jsonlite)

## Visits Data
visits_file_list <- list.files("~/Desktop/Gitcoin Sybil LEGOs/data_input/visits",full.names=TRUE)
visit_df <- funique(do.call(rbind,lapply(visits_file_list,function(filename) read_csv(filename)[,c( "id","handle","ip_address","created_on","modified_on")])))

## All Handles
all_handles <- unique(visit_df$handle)
all_handles_spl <- split(all_handles, ceiling(seq_along(all_handles)/2500))
gc()

## For evvery profile function to get score
get_ipscore <- function(handle,visit_df)
{
	tdata <- visit_df[visit_df$handle %in% handle,]
	t2data <- visit_df[visit_df$ip_address %in% tdata$ip_address & visit_df$handle != handle,]
	t2data_ip <- split(t2data,t2data$ip_address)
	ipdf <- data.frame(
						Handle = handle,
						IPCount = length(unique(tdata$ip_address)),
						IPCount_ProfShared_gt0 = sum(unique(tdata$ip_address) %in% unique(t2data$ip_address)),
						IPCount_ProfShared_gt1 = ifelse(nrow(t2data)==0,0,sum(sapply(t2data_ip,function(x) length(unique(x$handle))>1))),
						IPCount_ProfShared_gt3 = ifelse(nrow(t2data)==0,0,sum(sapply(t2data_ip,function(x) length(unique(x$handle))>3))),
						IPProfMatch = NA
					)
	if(nrow(t2data)>0)
	{
		ipdft <- do.call(rbind,lapply(t2data_ip,function(x) unique(x[,c("ip_address","handle")])))
		rownames(ipdft) <- NULL
		names(ipdft) <- c("IP","Handle")
		ipdf$IPProfMatch <- toJSON(ipdft)
	}
	return(ipdf)
}

## Run in looped and parallel
ipscores_dft <- list()
for(idx in 1:length(all_handles_spl))
{
	time_start <- Sys.time()
	ipscores_t <- mclapply(all_handles_spl[[idx]],get_ipscore,visit_df=visit_df,mc.cores=19)
	ipscores_dft[[idx]] <- do.call(rbind,ipscores_t)
	time_end <- Sys.time()
	message(paste0(idx,"/",length(all_handles_spl),":Took ",round(difftime(time_end,time_start,units="mins"),2)," minutes"))
}
ipscores_df <- do.call(rbind,ipscores_dft)

## Write out data
write_csv(ipscores_df[,1:5],"~/Desktop/Gitcoin Sybil LEGOs/data_processed/IPShared_scores.csv")
write_csv(ipscores_df,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/IPShared_matches.csv")

## Write data as fst file
library(fst)
ipscores_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/IPShared_matches.csv")
write_fst(ipscores_df, "~/Desktop/Gitcoin Sybil LEGOs/data_processed/IPShared_matches.fst")


