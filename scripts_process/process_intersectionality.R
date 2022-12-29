library(jsonlite)
library(readr)

## All Jsons
poap <- fromJSON("~/Desktop/Gitcoin Sybil LEGOs/data_input/graphs/poaps.json")
poh <- fromJSON("~/Desktop/Gitcoin Sybil LEGOs/data_input/graphs/poh.json")
lens <- fromJSON("~/Desktop/Gitcoin Sybil LEGOs/data_input/graphs/lens.json")
nfts <- fromJSON("~/Desktop/Gitcoin Sybil LEGOs/data_input/graphs/nfts.json")
snap <- fromJSON("~/Desktop/Gitcoin Sybil LEGOs/data_input/graphs/snapshot.json")

## Superset
final <- data.frame(address = unique(tolower(poap$wallets)))

## POAP Data append
final$poap <- as.numeric(poap$data$tokensOwned[match(final$address,tolower(poap$data$id))])
final$poap_count <- ifelse(is.na(final$poap),0,final$poap)
final$poap <- ifelse(is.na(final$poap),0,1)

## LENS Data append
final$lens <- as.numeric(!is.na(match(final$address,tolower(lens$data$ownedBy))))
final$lens_followers <- as.numeric(lens$data$stats$totalFollowers[match(final$address,tolower(lens$data$ownedBy))])
final$lens_following <- as.numeric(lens$data$stats$totalFollowing[match(final$address,tolower(lens$data$ownedBy))])

## POH
final$poh <- as.numeric(final$address %in% tolower(poh))

## NFTs
nftstemp <- data.frame(address = names(table(tolower(nfts$data$token$owner))), nfts = as.numeric(table(tolower(nfts$data$token$owner))))
final$nft_count <- nftstemp$nfts[match(final$address,nftstemp$address)]
final$nft <- ifelse(is.na(final$nft_count),0,1)

## Snapshot
snaptemp <- data.frame(address = names(table(tolower(snap$data$voter))), votes = as.numeric(table(tolower(snap$data$voter))))
final$snapshot_votes <- snaptemp$votes[match(final$address,snaptemp$address)]
final$snapshot <- ifelse(is.na(final$snapshot_votes),0,1)

## Score
final$Score <- final$poap+final$lens+final$poh+final$nft+final$snapshot

## Map Handle
wall_handles <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_input/gr15_contributions/20220923-001354_wallets.csv")
final <-cbind(Handle=wall_handles$handle[match(final$address,wall_handles$address)],final)

## Write out final file
write_csv(final,"~/Desktop/Gitcoin Sybil LEGOs/data_processed/Intersectionality.csv")