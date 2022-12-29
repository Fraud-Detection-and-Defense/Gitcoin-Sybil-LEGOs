library(readr)
library(networkD3)
library(igraph)

## Make Cluster Data
legodatafull <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_app/LegoDataFull.csv",show_col_types = FALSE)
metadistmat <- readRDS("~/Desktop/Gitcoin Sybil LEGOs/data_processed/Metamat_matches.RDS")
nn_nodes <- data.frame(
						name=rownames(metadistmat),
						group="Node",
						size=1
					)
match_list <- apply(metadistmat,1,function(x) names(which(x<=1)),simplify=FALSE)

make_links <- function(matches,handle,nodes)
{
	if(length(matches)==0) return(data.frame())
	data.frame(
				source = match(handle,nodes)-1,
				target = match(matches,nodes)-1,
				value = 1
			)
}
nn_links <- do.call(rbind,mapply(make_links,match_list,nn_nodes$name,MoreArgs=list(nodes=nn_nodes$name),SIMPLIFY=FALSE,USE.NAMES=FALSE))
network <- graph_from_edgelist(cbind(nn_links$source+1,nn_links$target+1))
network_c <- components(network)
nn_nodes$cluster <- network_c$membership
nn_nodes$clustersize <- network_c$csize[nn_nodes$cluster]
cluster_stats <- data.frame(
								ClusterId = 1:network_c$no,
								ClusterSize = network_c$csize,
								AvgGrantsDonated = ceiling(tapply(legodatafull$DDNAGrantsDonated[match(nn_nodes$name,legodatafull$Handle)],nn_nodes$cluster,mean))
							)
cluster_stats <- cluster_stats[with(cluster_stats, order(-ClusterSize,-AvgGrantsDonated)),]
saveRDS(nn_nodes,"~/Desktop/Gitcoin Sybil LEGOs/data_app/sybil_nodes.RDS")
saveRDS(cluster_stats,"~/Desktop/Gitcoin Sybil LEGOs/data_app/sybil_clusters.RDS")
