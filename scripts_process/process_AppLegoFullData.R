library(readr)
library(Rfast)

## Read in Handles
legodata <- data.frame(Handle=read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/gr15_contributions/20220923-001354_handles.csv")[,1,drop=TRUE])

## Lev Data
lev_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_scores.csv")
legodata$LevDistance <- lev_df$Distance_Min[match(legodata$Handle,lev_df$Handle)]
legodata$LevMatchCount <- lev_df$Distance_Min_is1_Count[match(legodata$Handle,lev_df$Handle)]

## Intersectionality
inter_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Intersectionality.csv")
legodata$IntersectionalityScore <- inter_df$Score[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityPOAPActive <- inter_df$poap[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityPOAPCount <- inter_df$poap_count[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityLensActive <- inter_df$lens[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityLensFollowers <- inter_df$lens_followers[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityLensFollowing <- inter_df$lens_following[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityPOHActive <- inter_df$poh[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityNFTActive <- inter_df$nft[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalityNFTCount <- inter_df$nft_count[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalitySnapshotActive <- inter_df$snapshot[match(legodata$Handle,inter_df$Handle)]
# legodata$IntersectionalitySnapshotVotes <- inter_df$snapshot_votes[match(legodata$Handle,inter_df$Handle)]

## DDNA
ddna_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_scores.csv")
data_mat_handles <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_binmat_handles.RDS")
data_mat_handles <- data_mat_handles[,!grepl("Amt",colnames(data_mat_handles))]
data_sum_handles <- data.frame(Handle = rownames(data_mat_handles),DDNAGrantsDonated=rowsums(data_mat_handles))
legodata$DDNADistance <- round(ddna_df$Distance_Min[match(legodata$Handle,ddna_df$Handle)],2)
legodata$DDNAGrantsDonated <- data_sum_handles$DDNAGrantsDonated[match(legodata$Handle,data_sum_handles$Handle)]

## APU
apu_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/APU.csv")
legodata$APUScore <- round(apu_df$Score[match(legodata$Handle,apu_df$Handle)],2)
legodata$APUScore <- ifelse(is.na(legodata$APUScore),0,legodata$APUScore)

## SAD
sad_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/SAD.csv")
legodata$SADScore <- round(sad_df$Score[match(legodata$Handle,sad_df$Handle)],2)

## GrantOwner
owner_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/grants_owners.csv")
legodata$GrantOwner <- ifelse(legodata$Handle %in% owner_df$handle,1,0)

## IPs Shared
ip_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/IPShared_scores.csv")
ip_df$Score <- ip_df$IPCount_ProfShared_gt1/ip_df$IPCount
legodata$IPsSharedRatio <- round(ip_df$Score[match(legodata$Handle,ip_df$Handle)],2)
legodata$IPsUsedCount <- round(ip_df$IPCount[match(legodata$Handle,ip_df$Handle)],2)
legodata$IPsSharedCount <- ip_df$IPCount_ProfShared_gt1[match(legodata$Handle,ip_df$Handle)]

## Meta Rank
get_rank_Loki <- function(x,type="lb",wt=1)
{
	if(type=="lb") return(wt*rank(x,na.last=TRUE))
	if(type=="lg") return(wt*rank(-1*x,na.last=TRUE))
}
get_rank_Thor <- function(x,type="lb",wt=1)
{
	if(type=="lb") return(wt*rank(x,na.last=FALSE))
	if(type=="lg") return(wt*rank(-1*x,na.last=FALSE))
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
legodata_spl <- as.list(legodata)[2:12]
legodata$MetaRank_Loki <- range01(apply(mapply(get_rank_Loki,legodata_spl,type=c("lb","lg","lb","lb","lb","lb","lg","lb","lg","lg","lg"),wt=rep(1,11)),1,sum))
legodata$MetaRank_Thor <- range01(apply(mapply(get_rank_Thor,legodata_spl,type=c("lb","lg","lb","lb","lb","lb","lg","lb","lg","lg","lg"),wt=rep(1,11)),1,sum))

# data.frame(Var=names(legodata_spl),RankType=c("lb","lg","lb","lb","lb","lg","lb","lg","lg","lg"))

## Write Out Data
write_csv(legodata,"~/Desktop/Gitcoin Sybil LEGOs/data_app/LegoDataFull.csv")