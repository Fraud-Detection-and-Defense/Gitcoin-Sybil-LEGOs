library(readr)
library(kit)
library(parallel)
library(Rfast)
library(stringdist)

## DDNA Mat
dist_mat_handle_jacc <- as.matrix(readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_handles.RDS"))
dnadistmat <- (dist_mat_handle_jacc!=0)*1
saveRDS(dnadistmat,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_DNA_handles.RDS")

## Visits Data
dist_mat_handle_jacc <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_handles.RDS")
ddna_handles <- attributes(dist_mat_handle_jacc)$Labels
rm(dist_mat_handle_jacc);gc()
visits_file_list <- list.files("~/Desktop/Gitcoin Sybil LEGOs/data_input/visits",full.names=TRUE)
visit_df <- funique(do.call(rbind,lapply(visits_file_list,function(filename) read_csv(filename)[,c( "handle","ip_address")])))
ipdistmatl <- list()
for(idx in 1:length(ddna_handles))
{
	ipdistmatl[[idx]] <- as.numeric(!ddna_handles %in% visit_df$handle[visit_df$ip_address %in% visit_df$ip_address[visit_df$handle==ddna_handles[idx]]])
	if((idx%%1000)==0) message(idx)
}
ipdistmat <- do.call(rbind,ipdistmatl)
rownames(ipdistmat) <- ddna_handles
colnames(ipdistmat) <- ddna_handles
diag(ipdistmat) <- 0
saveRDS(ipdistmat,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_IP_handles.RDS")

## Lev Data
dist_mat_handle_jacc <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_handles.RDS")
ddna_handles <- attributes(dist_mat_handle_jacc)$Labels
rm(dist_mat_handle_jacc);gc()
levmat <- stringdistmatrix(ddna_handles,ddna_handles,method="lv")
levdist <- (levmat>1)*1
rownames(levdist) <- ddna_handles
colnames(levdist) <- ddna_handles
saveRDS(levdist,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_Lev_handles.RDS")

## MetaMat
dnadistmat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_DNA_handles.RDS")
ipdistmat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_IP_handles.RDS")
levdist <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_Lev_handles.RDS")
metadistmat <- dnadistmat+ipdistmat+levdist
saveRDS(metadistmat,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_sum_handles.RDS")

## Create only matched distmat
metadistmat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_sum_handles.RDS")
diag(metadistmat) <- 3
match_idx <- apply(metadistmat,1,function(x) any(x<=1))
metadistmat <- metadistmat[match_idx,match_idx]
saveRDS(metadistmat,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_matches.RDS")
gc()