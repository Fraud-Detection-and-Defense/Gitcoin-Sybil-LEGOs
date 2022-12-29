########################################################################
## Lev Network Plot
########################################################################
levn_split_nodes <- function(x)
{
	if(is.na(x)) return(NULL)
	unlist(strsplit(x,";;;"))
}
levn_make_links <- function(matches,handle,nodes)
{
	if(is.null(matches)) return(data.frame(source=numeric(),target=numeric(),value=numeric()))
	data.frame(
				source = match(handle,nodes)-1,
				target = match(matches,nodes)-1,
				value=1
			)
}
########################################################################
########################################################################


########################################################################
## DDNA Network Plot
########################################################################
fromJSON_ddna <- function(x,cutoff)
{
	xdf <- fromJSON(x)
	if(length(xdf)==0) return(list())
	xdf <- xdf[xdf$Jaccard<=cutoff,]
	if(nrow(xdf)==0) return(list())
	xdf
}
ddna_make_links <- function(matches,handle,nodes)
{
	if(length(matches)==0) return(data.frame(source=numeric(),target=numeric(),value=numeric()))
	data.frame(
				source = match(handle,nodes)-1,
				target = match(matches$Handle,nodes)-1,
				Jaccard = matches$Jaccard,
				value = 1
			)
}
########################################################################
########################################################################


########################################################################
## IPs Network Plot
########################################################################
fromJSON_range <- function(x)
{
	if(is.na(x)) return(list())
	xdf <- fromJSON(x)
	xdf_spl <- split(xdf,xdf$IP)
	max(sapply(xdf_spl,nrow))
}
fromJSON_NA <- function(x,cutoff1,cutoff2)
{
	if(is.na(x)) return(list())
	xdf <- fromJSON(x)
	xdf_spl <- split(xdf,xdf$IP)
	xdf_spl <- xdf_spl[sapply(xdf_spl,nrow)>=cutoff1 & sapply(xdf_spl,nrow)<=cutoff2]
	do.call(rbind,xdf_spl)
}
ips_make_links <- function(matches,handle,nodes)
{
	if(length(matches)==0) return(data.frame(source=numeric(),target=numeric(),value=numeric()))
	
	rbind(
			data.frame(
							source = match(handle,nodes)-1,
							target = match(unique(matches$IP),nodes)-1,
							value = .1
					),
			data.frame(
							source = match(matches$IP,nodes)-1,
							target = match(matches$Handle,nodes)-1,
							value = .1
					)
	)
}
########################################################################
########################################################################


########################################################################
## GDNA Network Plot
gdna_make_links <- function(matches,handle,nodes)
{
	if(length(matches)==0) return(data.frame(source=numeric(),target=numeric(),value=numeric()))
	data.frame(
				source = match(handle,nodes)-1,
				target = match(matches$Grant,nodes)-1,
				Jaccard = matches$Jaccard,
				value = 1
			)
}
########################################################################
########################################################################


########################################################################
## GDNA Donor DDNA Network Plot
########################################################################
grant_ddna_make_links <- function(matches,handle,nodes,cutoff)
{
	matches <- fromJSON(matches)
	matches <- matches[matches$Jaccard <= cutoff & matches$Handle %in% nodes,]
	if(nrow(matches)==0) return(data.frame())
	data.frame(
				source = match(handle,nodes)-1,
				target = match(matches$Handle,nodes)-1,
				Jaccard = matches$Jaccard,
				value = 1
			)
}
########################################################################
########################################################################


########################################################################
## Output Filter criterion
########################################################################
print_filter <- function(x)
{
	if(x$con ==1)
	{
		con_part <- paste0(x$var," == ",x$cutoff) 
		miss_con <- ifelse(x$miss==1," OR "," AND ")
		miss_part <- ifelse(x$miss==1,paste0(x$var," is NA"),paste0(x$var," is not NA"))
	}
	if(x$con ==2)
	{
		con_part <- paste0(x$var," <= ",x$cutoff) 
		miss_con <- ifelse(x$miss==1," OR "," AND ")
		miss_part <- ifelse(x$miss==1,paste0(x$var," is NA"),paste0(x$var," is not NA"))
	}
	if(x$con ==3)
	{
		con_part <- paste0(x$var," >= ",x$cutoff) 
		miss_con <- ifelse(x$miss==1," OR "," AND ")
		miss_part <- ifelse(x$miss==1,paste0(x$var," is NA"),paste0(x$var," is not NA"))
	}
	if(x$con ==4)
	{
		con_part <- paste0(x$var," >= ",x$cutoff[1]," AND ",x$var," <= ",x$cutoff[2]) 
		miss_con <- ifelse(x$miss==1," OR "," AND ")
		miss_part <- ifelse(x$miss==1,paste0(x$var," is NA"),paste0(x$var," is not NA"))
	}
	if(x$con ==5)
	{
		con_part <- paste0(x$var," <= ",x$cutoff[1]," OR ",x$var," >= ",x$cutoff[2]) 
		miss_con <- ifelse(x$miss==1," OR "," AND ")
		miss_part <- ifelse(x$miss==1,paste0(x$var," is NA"),paste0(x$var," is not NA"))
	}
	paste0("[(",con_part,")",miss_con,miss_part,"]",collapse="")
}
process_filter <- function(x,data)
{
	if(x$con ==1)
	{
		con_part <- data[,x$var] == x$cutoff
		if(x$miss==1) return(con_part | is.na(data[,x$var]))
		if(x$miss==2) return(con_part & !is.na(data[,x$var]))
	}
	if(x$con ==2)
	{
		con_part <- data[,x$var] <= x$cutoff
		if(x$miss==1) return(con_part | is.na(data[,x$var]))
		if(x$miss==2) return(con_part & !is.na(data[,x$var]))
	}
	if(x$con ==3)
	{
		con_part <- data[,x$var] >= x$cutoff
		if(x$miss==1) return(con_part | is.na(data[,x$var]))
		if(x$miss==2) return(con_part & !is.na(data[,x$var]))
	}
	if(x$con ==4)
	{
		con_part <- (data[,x$var] >= x$cutoff[1]) & (data[,x$var] <= x$cutoff[2])
		if(x$miss==1) return(con_part | is.na(data[,x$var]))
		if(x$miss==2) return(con_part & !is.na(data[,x$var]))
	}
	if(x$con ==5)
	{
		con_part <- (data[,x$var] <= x$cutoff[1]) | (data[,x$var] >= x$cutoff[2])
		if(x$miss==1) return(con_part | is.na(data[,x$var]))
		if(x$miss==2) return(con_part & !is.na(data[,x$var]))
	}
}
########################################################################
########################################################################


