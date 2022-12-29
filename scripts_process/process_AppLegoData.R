library(readr)

## Read in Handles
legodata <- data.frame(Handle=read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/gr15_contributions/20220923-001354_handles.csv")[,1,drop=TRUE])

## Lev Data
lev_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Levenshtein_scores.csv")
legodata$LevDistance <- lev_df$Distance_Min[match(legodata$Handle,lev_df$Handle)]

## Intersectionality
inter_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Intersectionality.csv")
legodata$IntersectionalityScore <- inter_df$Score[match(legodata$Handle,inter_df$Handle)]

## DDNA
ddna_df <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Jaccard_handles_scores.csv")
legodata$DDNADistance <- round(ddna_df$Distance_Min[match(legodata$Handle,ddna_df$Handle)],2)

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
legodata$IPsShared <- round(ip_df$Score[match(legodata$Handle,ip_df$Handle)],2)

## Write Out Data
write_csv(legodata,"~/Desktop/Gitcoin Sybil LEGOs/data_app/LegoData.csv")