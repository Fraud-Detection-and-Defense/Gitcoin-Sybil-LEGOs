###########################################################################
## Load raw data clean it and save for further processing
###########################################################################
library(readr)
data <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/gr15_contributions/20220923-001354_contributions_gr15.csv")
data <- data[data$grant_id!=12,]
data <- data[data$handle!="gitcoinbot",]
###########################################################################
###########################################################################


###########################################################################
## Create Binary Matrix for both grants and handels
###########################################################################
all_grants <- sort(unique(data$grant_id))
all_users <- sort(unique(data$handle))
data_split <- split(data,data$grant_id)
matlist <- list()
for(idx in 1:length(all_grants))
{
	t_data <- data_split[[as.character(all_grants[idx])]]
	matlist[[idx]] <- as.numeric(all_users %in% t_data$handle)
	message(idx)
}
data_mat <- do.call(rbind,matlist)
rownames(data_mat) <- as.character(all_grants)
colnames(data_mat) <- all_users
saveRDS(data_mat,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_grants.RDS")
data_mat <- t(data_mat)
gc()

## Append Amount donated in Binary
amtdf <- data.frame(handle=all_users)
amtlst <- list()
for(idx in 1:nrow(amtdf))
{
  amtt <- sum(data[data$handle==amtdf$handle[idx],]$amount_in_usdt)
  amtlst[[idx]] <- as.numeric(c(amtt==0,amtt > c(1,2^(1:11))))
	if((idx %% 1000)==0) message(idx)
}
amtdft <- do.call(rbind,amtlst)
colnames(amtdft) <- paste0("Amt_",c(0,1,2^(1:11)))
data_mat_amt <- cbind(data_mat,amtdft)
saveRDS(data_mat_amt,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_handles.RDS")

## Write data as fst file
library(fst)
data_mat_amt <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_handles.RDS")
write_fst(as.data.frame(data_mat_amt), "~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_handles.fst")
data_mat_amt <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_grants.RDS")
write_fst(as.data.frame(data_mat_amt), "~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_grants.fst")
###########################################################################
###########################################################################


###########################################################################
## Create Distance Matrix 
###########################################################################
## Handle Wise Distance Matrix
library(parallelDist)
data_mat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_handles.RDS")
dist_mat_handle_jacc <- parDist(data_mat,method="binary")
saveRDS(dist_mat_handle_jacc,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_handles.RDS")

## Grant Wise Distance Matrix
library(parallelDist)
data_mat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_grants.RDS")
dist_mat_grant_jacc <- parDist(data_mat,method="binary")
saveRDS(dist_mat_grant_jacc,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_grants.RDS")
###########################################################################
###########################################################################


###########################################################################
## Jaccard Min Distance Donors
###########################################################################
f <- function (i, j, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (i >= 1) & (j >= 1) & (i > j) & (i <= n) & (j <= n)
  k <- (2 * n - j) * (j - 1) / 2 + (i - j)
  k[!valid] <- NA_real_
  k
}
SliceExtract_dist <- function (dist_obj, k) {
  if (length(k) > 1) stop("The function is not 'vectorized'!")
  n <- attr(dist_obj, "Size")
  if (k < 1 || k > n) stop("k out of bound!")
  ##
  i <- 1:(k - 1)
  j <- rep.int(k, k - 1)
  v1 <- dist_obj[f(j, i, dist_obj)]
  ## 
  i <- (k + 1):n
  j <- rep.int(k, n - k)
  v2 <- dist_obj[f(i, j, dist_obj)]
  ## 
  c(v1, 0, v2)
}
options(scipen=999)
library(readr)
library(jsonlite)
dist_mat_handle_jacc <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_handles.RDS")
dist_mat_handle_jacc_labels <- attributes(dist_mat_handle_jacc)$Labels
mindist <- data.frame(Handle = dist_mat_handle_jacc_labels, Distance_Min = NA, Handle_Match = NA)
for(idx in 1:nrow(mindist))
{
	slicedata <- data.frame(
                            Handle = dist_mat_handle_jacc_labels[-idx],
                            Jaccard = SliceExtract_dist(dist_mat_handle_jacc, idx)[-idx]
                          )
  mindist$Distance_Min[idx] <- min(slicedata$Jaccard)
  slicedata <- slicedata[slicedata$Jaccard <= .25,]
  mindist$Handle_Match[idx] <- toJSON(slicedata)
	if((idx %% 1000)==0) message(paste0(idx,"/",nrow(mindist)))
}
readr::write_csv(mindist[,-3],"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_scores.csv")
readr::write_csv(mindist,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_matches.csv")

## Write data as fst file
library(fst)
mindist <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_matches.csv")
write_fst(mindist, "~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_matches.fst")
###########################################################################
###########################################################################


###########################################################################
## Jaccard Min Distance Grants
###########################################################################
f <- function (i, j, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (i >= 1) & (j >= 1) & (i > j) & (i <= n) & (j <= n)
  k <- (2 * n - j) * (j - 1) / 2 + (i - j)
  k[!valid] <- NA_real_
  k
}
SliceExtract_dist <- function (dist_obj, k) {
  if (length(k) > 1) stop("The function is not 'vectorized'!")
  n <- attr(dist_obj, "Size")
  if (k < 1 || k > n) stop("k out of bound!")
  ##
  i <- 1:(k - 1)
  j <- rep.int(k, k - 1)
  v1 <- dist_obj[f(j, i, dist_obj)]
  ## 
  i <- (k + 1):n
  j <- rep.int(k, n - k)
  v2 <- dist_obj[f(i, j, dist_obj)]
  ## 
  c(v1, 0, v2)
}
options(scipen=999)
library(readr)
dist_mat_grant_jacc <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_distmat_grants.RDS")
dist_mat_grant_jacc_labels <- attributes(dist_mat_grant_jacc)$Labels
mindist <- data.frame(GrantId = dist_mat_grant_jacc_labels, Distance_Min = NA, Grant_Match = NA)
for(idx in 1:nrow(mindist))
{
  slicedata <- data.frame(
                            Grant = dist_mat_grant_jacc_labels[-idx],
                            Jaccard = SliceExtract_dist(dist_mat_grant_jacc, idx)[-idx]
                          )
  mindist$Distance_Min[idx] <- min(slicedata$Jaccard)
  slicedata <- slicedata[slicedata$Jaccard <= .25,]
  mindist$Grant_Match[idx] <- toJSON(slicedata)
  if((idx %% 100)==0) message(paste0(idx,"/",nrow(mindist)))
}
readr::write_csv(mindist[,-3],"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_grants_scores.csv")
readr::write_csv(mindist,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_grants_matches.csv")

## Write data as fst file
library(fst)
mindist <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_grants_matches.csv")
write_fst(mindist, "~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_grants_matches.fst")
###########################################################################
###########################################################################