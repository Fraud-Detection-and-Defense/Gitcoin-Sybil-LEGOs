## Loading Libraries
library(shiny)
library(shinydashboard)
library(readr)
library(fst)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(omnitheme)
library(ggpubr)
library(RColorBrewer)
library(DT)
library(networkD3)
library(igraph)
library(parallel)
options(scipen=999)

########################################################################
## Help Commands
########################################################################
# setwd("~/Desktop/Gitcoin Sybil LEGOs");shiny::runApp(port=7551)
# message(paste0("Debug======",data_ips_matches))
########################################################################
########################################################################


########################################################################
## Load All data and Functions
########################################################################
## Load Helper Functions
source("app_helpers.R")

## Load Lego Data
legodata <- read_csv("data_app/LegoData.csv",show_col_types = FALSE)
Plot_Lev <- readRDS("data_app/Plot_Lev.RDS")
Plot_Intersectionality <- readRDS("data_app/Plot_Intersectionality.RDS")
Plot_DDNA <- readRDS("data_app/Plot_DDNA.RDS")
Plot_APU <- readRDS("data_app/Plot_APU.RDS")
Plot_SAD <- readRDS("data_app/Plot_SAD.RDS")
Plot_IPs <- readRDS("data_app/Plot_IPs.RDS")

## Read Data for Lev Panel
data_lev_scores <- read_csv("data_processed/Levenshtein_scores.csv",show_col_types = FALSE)
data_lev_matches <- "data_processed/Levenshtein_matches.fst"

## Read Data for DDNA Panel
data_ddna_scores <- read_csv("data_processed/Jaccard_handles_scores.csv",show_col_types = FALSE)
data_ddna_matches <- "data_processed/Jaccard_handles_matches.fst"
data_ddna_matches_loaded <- read_fst(data_ddna_matches)
data_ddna_binmat <- "data_processed/Jaccard_binmat_handles.fst"

## Read Data for GDNA Panel
owner_df <- read_csv("data_processed/grants_owners.csv",show_col_types = FALSE)
data_gdna_scores <- read_csv("data_processed/Jaccard_grants_scores.csv",show_col_types = FALSE)
data_gdna_matches <- "data_processed/Jaccard_grants_matches.fst"
data_gdna_binmat <- "data_processed/Jaccard_binmat_grants.fst"

## Read Data for IPs Panel
data_ips_scores <- read_csv("data_processed/IPShared_scores.csv",show_col_types = FALSE)
data_ips_matches <- "data_processed/IPShared_matches.fst"

## Read Data for Filtering and Export
legodatafull <- read_csv("data_app/LegoDataFull.csv",show_col_types = FALSE)

## Sybil Clusters
sybil_nodes <- readRDS("data_app/sybil_nodes.RDS")
sybil_clusters <- readRDS("data_app/sybil_clusters.RDS")
########################################################################
########################################################################


########################################################################
## Server Code
########################################################################
function(input, output, session) {

    ########################################################################
    ## Handle Inspection
    ########################################################################
    ## Handle UI
    updateSelectizeInput(session, 'handle_sel', choices = legodata$Handle, selected=c("omnianalytics","disruptionjoe","owocki"), server = TRUE,options = list(maxItems = 5))

    ## LEGO Score Plots
    output$legoplot <- renderPlot({
                                    ## If NULL
                                    if(length(input$handle_sel)==0) return(NULL)

                                    ## Selected Handles Data Frame
                                    sel_df <- legodata[match(input$handle_sel,legodata$Handle),]
                                    
                                    ## Lev Plot
                                    y_lim1 <- c(0,15000)
                                    y_seq1 <- seq(y_lim1[1],y_lim1[2],length.out=nrow(sel_df)+2)
                                    sel_df1 <- sel_df
                                    sel_df1$LevDistance <- ifelse(sel_df1$LevDistance>5,6,sel_df1$LevDistance)
                                    p1 <-   Plot_Lev+
                                            geom_point(data = sel_df1, mapping = aes(x = LevDistance, y = y_seq1[c(-1,-length(y_seq1))],shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df1)])

                                    ## Intersectionality
                                    y_lim2 <- c(0,12000)
                                    y_seq2 <- seq(y_lim2[1],y_lim2[2],length.out=nrow(sel_df)+2)
                                    p2 <-   Plot_Intersectionality+
                                            geom_point(data = sel_df, mapping = aes(x = IntersectionalityScore, y = y_seq2[c(-1,-length(y_seq2))], shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df)])

                                    ## DDNA
                                    y_lim3 <- c(0,22000)
                                    y_seq3 <- seq(y_lim3[1],y_lim3[2],length.out=nrow(sel_df)+2)
                                    p3 <-   Plot_DDNA+
                                            geom_point(data = sel_df, mapping = aes(x = DDNADistance, y = y_seq3[c(-1,-length(y_seq3))], shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df)])

                                    ## APU
                                    y_lim4 <- c(0,4000)
                                    y_seq4 <- seq(y_lim4[1],y_lim4[2],length.out=nrow(sel_df)+2)
                                    p4 <-   Plot_APU+
                                            geom_point(data = sel_df, mapping = aes(x = APUScore, y = y_seq4[c(-1,-length(y_seq4))], shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df)])

                                    ## SAD
                                    y_lim5 <- c(0,30000)
                                    y_seq5 <- seq(y_lim5[1],y_lim5[2],length.out=nrow(sel_df)+2)
                                    p5 <-   Plot_SAD+
                                            geom_point(data = sel_df, mapping = aes(x = SADScore, y = y_seq5[c(-1,-length(y_seq5))], shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df)])

                                    ## IPs
                                    y_lim6 <- c(0,18000)
                                    y_seq6 <- seq(y_lim6[1],y_lim6[2],length.out=nrow(sel_df)+2)
                                    p6 <-   Plot_IPs+
                                            geom_point(data = sel_df, mapping = aes(x = IPsShared, y = y_seq6[c(-1,-length(y_seq6))], shape = Handle,fill = Handle,color=Handle),size = 5,na.rm=TRUE)+
                                            scale_shape_manual(values = c(21, 22, 23, 24, 25)[1:nrow(sel_df)])
                                    
                                    suppressWarnings(
                                    ggarrange( 
                                                p1,p2,p3,p4,p5,p6,
                                                labels = NULL,
                                                ncol = 3, nrow = 2,
                                                common.legend = TRUE,
                                                legend = "top"
                                    ))
                        }, bg="transparent")

    ## Lev Network Plot
    output$lev_network <- renderForceNetwork({
                                                ## If NULL
                                                if(length(input$handle_sel)==0) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_handles <- input$handle_sel
                                                nn_json <- lapply(match(nn_handles,data_lev_scores$Handle),function(x) read_fst(data_lev_matches, from=x, to=x)$Distance_Min_is1_Matches)
                                                nn_nodesl <- lapply(nn_json,levn_split_nodes)
                                                node_names <- unique(c(nn_handles,unlist(nn_nodesl)))
                                                nn_nodes <- data.frame(
                                                                            name = node_names,
                                                                            group = ifelse(node_names %in% nn_handles,"Handle","Match"),
                                                                            size = ifelse(node_names %in% nn_handles,10,1)
                                                                        )
                                                nn_links <- do.call(rbind,mapply(levn_make_links,nn_nodesl,nn_handles,MoreArgs=list(nodes=nn_nodes$name),SIMPLIFY=FALSE))
                                                if(length(nn_handles)>1) nn_links <- rbind(data.frame(source=0,target=1,value=0),nn_links)
                                                if(nrow(nn_links)==0) stop("No Matches to display")
                                                

                                                ## Network Plot
                                                my_color <- 'd3.scaleOrdinal() .domain(["Handle", "Match"]) .range(["#76b7b2","#e15759"])'
                                                forceNetwork(
                                                                Links = nn_links,
                                                                Nodes = nn_nodes,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                colourScale=my_color,
                                                                legend = TRUE,
                                                                fontSize = 10,
                                                                opacityNoHover = 1,
                                                                bounded=TRUE
                                                            )
                            })

    ## DDNA Network Plot
    output$ddna_network <- renderForceNetwork({
                                                ## If NULL
                                                if(length(input$handle_sel)==0) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_handles <- input$handle_sel
                                                nn_json <- lapply(match(nn_handles,data_ddna_scores$Handle),function(x) read_fst(data_ddna_matches, from=x,to=x)$Handle_Match)
                                                nn_nodesl <- lapply(nn_json,fromJSON_ddna,cutoff = input$ddna_network_cutoff)
                                                node_names <- unique(c(nn_handles,unlist(lapply(nn_nodesl,function(x) x$Handle))))
                                                nn_nodes <- data.frame(
                                                                            name = node_names,
                                                                            group = ifelse(node_names %in% nn_handles,"Handle","Match"),
                                                                            size = ifelse(node_names %in% nn_handles,10,1)
                                                                        )
                                                nn_links <- do.call(rbind,mapply(ddna_make_links,nn_nodesl,nn_handles,MoreArgs=list(nodes=nn_nodes$name),SIMPLIFY=FALSE))
                                                if(length(nn_handles)>1) nn_links <- rbind(data.frame(source=0,target=1,Jaccard=1,value=0),nn_links)
                                                if(nrow(nn_links)==0) stop("No Matches to display")
                                                nn_links$color <- "#515A5A"
                                                nn_links$value <- 1
                                                nn_links$value <- ifelse(nn_links$Jaccard==0,16,nn_links$value)
                                                nn_links$color <- ifelse(nn_links$Jaccard==0,"#DC143C",nn_links$color)
                                                if(length(nn_handles)>1) nn_links$value[1] <- 0
                                                 
                                                ## Network Plot
                                                my_color <- 'd3.scaleOrdinal() .domain(["Handle", "Match"]) .range(["#76b7b2","#e15759"])'
                                                forceNetwork(
                                                                Links = nn_links,
                                                                Nodes = nn_nodes,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                colourScale=my_color,
                                                                linkColour = nn_links$color,
                                                                legend = TRUE,
                                                                fontSize = 10,
                                                                opacityNoHover = 1,
                                                                bounded=TRUE
                                                            )
                            })

    ## DDNA Account Inspection
    observeEvent(input$handle_sel,{updateSelectizeInput(session, 'ddna_network_handle', choices = input$handle_sel, selected=input$handle_sel[1], server = FALSE)})
    output$ddna_table <- renderDataTable({
                                            if(length(input$handle_sel)==0) return(NULL)

                                            ## Generate Table
                                            dt_handle <- input$ddna_network_handle
                                            dt_json <- read_fst(data_ddna_matches, from=match(dt_handle,data_ddna_scores$Handle),to=match(dt_handle,data_ddna_scores$Handle))$Handle_Match
                                            dt_matches <- fromJSON(dt_json)
                                            if(length(dt_matches)==0) stop("No Match to display for selected User")
                                            dt_matches <- c(dt_handle,dt_matches[dt_matches$Jaccard<=input$ddna_network_cutoff,"Handle"])
                                            if(length(dt_matches)==1) stop("No Match to display for selected User")
                                            dt_table <- t(do.call(rbind,lapply(match(dt_matches,data_ddna_scores$Handle),function(x) read_fst(data_ddna_binmat, from=x,to=x))))
                                            rownames(dt_table)[1497:1509] <- paste0(c("Amt"),c(" <",rep(" >",12))," $",c(1,1,2^(1:11)))
                                            dt_table <- dt_table[apply(dt_table,1,sum)>0,,drop=FALSE]
                                            dt_table <- as.data.frame(cbind(paste0(1:length(rownames(dt_table)),") ",rownames(dt_table)),dt_table))
                                            names(dt_table) <- sprintf('<div style="font-size:8px;">%s</div>', c("Grant/Amount",dt_matches))

                                            datatable(
                                                        dt_table,
                                                        escape = FALSE,
                                                        rownames=FALSE,
                                                        options = list(
                                                                        paging = TRUE,
                                                                        bInfo = FALSE,
                                                                        ordering=FALSE,
                                                                        searching=FALSE,
                                                                        autoWidth = TRUE,
                                                                        bLengthChange = FALSE,
                                                                        pageLength = 20,
                                                                        columnDefs = list(list(width = '3px', targets = c(1:length(dt_matches))))
                                                                    )
                                                    ) %>%
                                            formatStyle(columns = c(2:(length(dt_matches)+1)), backgroundColor = styleInterval(0,c("red","limegreen")), fontWeight = 'bold') %>%
                                            formatStyle(columns = c(2:(length(dt_matches)+1)), color = styleInterval(0,c("red","limegreen")), fontWeight = 'bold') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)), border = '1px solid #ddd') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)), fontSize = '8px') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)),lineHeight='10%') 
                                        })

    ## IPs Network Plot
    ips_network_cutoffs <- reactiveValues(cutoff1=1,cutoff2=20)
    observeEvent(input$handle_sel,{updateSelectizeInput(session, 'ips_network_handle', choices = input$handle_sel, selected=input$handle_sel[1], server = FALSE)})
    observeEvent(input$ips_network_handle,{
                                        if(length(input$handle_sel)!=0)
                                        {
                                            nn_handles <- input$ips_network_handle
                                            if(!is.na(match(nn_handles,data_ips_scores$Handle)))
                                            {
                                                nn_idx <- 1:length(nn_handles)-1
                                                nn_json <- lapply(match(nn_handles,data_ips_scores$Handle),function(x) read_fst(data_ips_matches, from=x, to=x)$IPProfMatch)
                                                nn_nodesl <- lapply(nn_json,fromJSON_range)
                                                cutoffmaxrange <- nn_nodesl[[1]]
                                                updateSliderInput(
                                                                    session,
                                                                    'ips_network_cutoff',
                                                                    min=1,
                                                                    max=ifelse(cutoffmaxrange<20,20,10*ceiling(cutoffmaxrange/10)),
                                                                    value=c(1,15),
                                                                    step=1
                                                                    )
                                            }
                                        }
    })
    observeEvent(input$ips_network_cutoff_update,{
                                        ips_network_cutoffs$cutoff1 <- input$ips_network_cutoff[1]
                                        ips_network_cutoffs$cutoff2 <- input$ips_network_cutoff[2]
    })
    output$ips_network <- renderForceNetwork({
                                                ## If NULL
                                                if(length(input$handle_sel)==0) return(NULL)
                                                if(length(input$ips_network_handle)==0) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_handles <- input$ips_network_handle
                                                if(is.na(match(nn_handles,data_ips_scores$Handle))) stop("No IP Data Exists for this account")
                                                nn_json <- lapply(match(nn_handles,data_ips_scores$Handle),function(x) read_fst(data_ips_matches, from=x, to=x)$IPProfMatch)
                                                nn_nodesl <- lapply(nn_json,fromJSON_NA,cutoff1=ips_network_cutoffs$cutoff1,cutoff2=ips_network_cutoffs$cutoff2)
                                                node_IPS <- unique(unlist(lapply(nn_nodesl,function(x) x$IP)))
                                                node_Handles <- unique(unlist(lapply(nn_nodesl,function(x) x$Handle)))
                                                node_names <- unique(c(nn_handles,node_IPS,node_Handles))
                                                nn_nodes <- data.frame(
                                                                            name = node_names,
                                                                            group = ifelse(node_names %in% nn_handles,"Handle",ifelse(node_names %in% node_IPS,"IP","Match")),
                                                                            size = ifelse(node_names %in% nn_handles,9,ifelse(node_names %in% node_IPS,1,1))
                                                                        )
                                                nn_links <- do.call(rbind,mapply(ips_make_links,nn_nodesl,nn_handles,MoreArgs=list(nodes=nn_nodes$name),SIMPLIFY=FALSE))
                                                if(length(nn_handles)>1) nn_links <- rbind(data.frame(source=0,target=1,value=0),nn_links)
                                                if(nrow(nn_links)==0) stop("No Matches to display")

                                                # message(paste0("Debug---------->",nrow(nn_links)))
                                                
                                                if(nrow(nn_links)>1000) stop("Network Too Big to render, Try limiting Cutoff Above")

                                                ## Network Plot
                                                my_color <- 'd3.scaleOrdinal() .domain(["Handle", "Match", "IP"]) .range(["#76b7b2","#e15759","#1F618D"])'
                                                forceNetwork(
                                                                Links = nn_links,
                                                                Nodes = nn_nodes,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                colourScale=my_color,
                                                                legend = TRUE,
                                                                fontSize = 10,
                                                                opacityNoHover = as.numeric(nrow(nn_nodes)<100),
                                                                bounded=FALSE,
                                                                charge=-10,
                                                                radiusCalculation = "Math.sqrt(d.nodesize)+4",
                                                                zoom=TRUE
                                                            )
                            })
    output$num_ips <- renderValueBox({
                                        if(length(input$ips_network_handle)==0) return(valueBox(value=NA,""))
                                        if(is.na(match(input$ips_network_handle,data_ips_scores$Handle))) return(valueBox(value=NA,""))
                                        valueBox(data_ips_scores$IPCount[match(input$ips_network_handle,data_ips_scores$Handle)], "IPs Used",width=NULL,color="olive")
                        })
    output$num_ips_shared <- renderValueBox({
                                                if(length(input$ips_network_handle)==0) return(valueBox(value=NA,""))
                                                if(is.na(match(input$ips_network_handle,data_ips_scores$Handle))) return(valueBox(value=NA,""))
                                                valueBox(data_ips_scores$IPCount_ProfShared_gt0[match(input$ips_network_handle,data_ips_scores$Handle)], "IPs Shared With Other Users",width=NULL,color="olive")
                        })
    output$num_ips_shared1 <- renderValueBox({
                                                if(length(input$ips_network_handle)==0) return(valueBox(value=NA,""))
                                                if(is.na(match(input$ips_network_handle,data_ips_scores$Handle))) return(valueBox(value=NA,""))
                                                valueBox(data_ips_scores$IPCount_ProfShared_gt1[match(input$ips_network_handle,data_ips_scores$Handle)], "IPs Shared With >1 Users",width=NULL,color="olive")
                        })
    output$num_ips_shared2 <- renderValueBox({
                                                if(length(input$ips_network_handle)==0) return(valueBox(value=NA,""))
                                                if(is.na(match(input$ips_network_handle,data_ips_scores$Handle))) return(valueBox(value=NA,""))
                                                valueBox(data_ips_scores$IPCount_ProfShared_gt3[match(input$ips_network_handle,data_ips_scores$Handle)], "IPs Shared With >3 Users",width=NULL,color="olive")
                        })
    ########################################################################
    ########################################################################

    
    ########################################################################
    ## Grant Inspection UI
    ########################################################################
    ## Grant UI
    updateSelectizeInput(session, 'grant_sel', choices = setNames(as.list(data_gdna_scores$GrantId), owner_df$title[match(data_gdna_scores$GrantId,owner_df$grant_id)]), selected=c("562","3591"), server = TRUE,options = list(maxItems = 5))

    ## GDNA Network Plot
    output$gdna_network <- renderForceNetwork({
                                                ## If NULL
                                                if(length(input$grant_sel)==0) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_handles <- input$grant_sel
                                                nn_json <- lapply(match(nn_handles,data_gdna_scores$GrantId),function(x) read_fst(data_gdna_matches, from=x,to=x)$Grant_Match)
                                                nn_nodesl <- lapply(nn_json,fromJSON)
                                                node_names <- unique(c(nn_handles,unlist(lapply(nn_nodesl,function(x) x$Grant))))
                                                nn_nodes <- data.frame(
                                                                            name = node_names,
                                                                            group = ifelse(node_names %in% nn_handles,"Grant","Match"),
                                                                            size = ifelse(node_names %in% nn_handles,10,1)
                                                                        )
                                                nn_links <- do.call(rbind,mapply(gdna_make_links,nn_nodesl,nn_handles,MoreArgs=list(nodes=nn_nodes$name),SIMPLIFY=FALSE))
                                                if(length(nn_handles)>1) nn_links <- rbind(data.frame(source=0,target=1,Jaccard=1,value=0),nn_links)
                                                if(nrow(nn_links)==0) stop("No Matches to display")
                                                nn_links$color <- ifelse(nn_links$Jaccard<=input$gdna_network_cutoff,"#F1948A","#CACFD2")
                                                nn_links$value <- ifelse(nn_links$Jaccard<=input$gdna_network_cutoff,10,.1)
                                                nn_links$value <- ifelse(nn_links$Jaccard==0,100,nn_links$value)
                                                if(length(nn_handles)>1) nn_links$value[1] <- 0
                                                nn_nodes$name2 <- owner_df$title[match(nn_nodes$name,owner_df$grant_id)]
                                                 
                                                ## Network Plot
                                                my_color <- 'd3.scaleOrdinal() .domain(["Grant", "Match"]) .range(["#76b7b2","#e15759"])'
                                                forceNetwork(
                                                                Links = nn_links,
                                                                Nodes = nn_nodes,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name2",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                colourScale=my_color,
                                                                linkColour = nn_links$color,
                                                                legend = TRUE,
                                                                fontSize = 10,
                                                                opacityNoHover = 1,
                                                                bounded=TRUE
                                                            )
                            })

    ## GDNA Grant Inspection
    observeEvent(input$grant_sel,{updateSelectizeInput(session, 'gdna_network_grant', choices = setNames(as.list(input$grant_sel), owner_df$title[match(input$grant_sel,owner_df$grant_id)]), selected=input$grant_sel[1], server = FALSE)})
    output$gdna_table <- renderDataTable({
                                            if(length(input$grant_sel)==0) return(NULL)

                                            ## Generate Table
                                            dt_handle <- input$gdna_network_grant
                                            dt_json <- read_fst(data_gdna_matches, from=match(dt_handle,data_gdna_scores$GrantId),to=match(dt_handle,data_gdna_scores$GrantId))$Grant_Match
                                            dt_matches <- fromJSON(dt_json)
                                            if(length(dt_matches)==0) stop("No Match to display for selected Grant")
                                            dt_matches <- c(dt_handle,dt_matches[dt_matches$Jaccard<=input$gdna_network_cutoff,"Grant"])
                                            if(length(dt_matches)==1) stop("No Match to display for selected Grant")
                                            dt_table <- t(do.call(rbind,lapply(match(dt_matches,data_gdna_scores$GrantId),function(x) read_fst(data_gdna_binmat, from=x,to=x))))
                                            dt_table <- dt_table[apply(dt_table,1,sum)>0,,drop=FALSE]
                                            dt_table <- as.data.frame(cbind(paste0(1:length(rownames(dt_table)),") ",rownames(dt_table)),dt_table))
                                            names(dt_table) <- sprintf('<div style="font-size:8px;">%s</div>', c("Donor",dt_matches))

                                            datatable(
                                                        dt_table,
                                                        escape = FALSE,
                                                        rownames=FALSE,
                                                        options = list(
                                                                        paging = TRUE,
                                                                        bInfo = FALSE,
                                                                        ordering=FALSE,
                                                                        searching=FALSE,
                                                                        autoWidth = TRUE,
                                                                        bLengthChange = FALSE,
                                                                        pageLength = 20,
                                                                        columnDefs = list(list(width = '3px', targets = c(1:length(dt_matches))))
                                                                    )
                                                    ) %>%
                                            formatStyle(columns = c(2:(length(dt_matches)+1)), backgroundColor = styleInterval(0,c("red","limegreen")), fontWeight = 'bold') %>%
                                            formatStyle(columns = c(2:(length(dt_matches)+1)), color = styleInterval(0,c("red","limegreen")), fontWeight = 'bold') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)), border = '1px solid #ddd') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)), fontSize = '8px') %>%
                                            formatStyle(columns = c(1:(length(dt_matches)+1)),lineHeight='10%') 
                                        })
    ## Grant LEGOs Inspection
    output$glegoplot <- renderPlot({
                                    ## If NULL
                                    if(length(input$grant_sel)==0) return(NULL)

                                    dt_grant <- input$grant_sel
                                    dt_grant2 <- owner_df$title[match(dt_grant,owner_df$grant_id)]
                                    han_idx <- lapply(match(dt_grant,data_gdna_scores$GrantId),function(x) names(which(unlist(read_fst(data_gdna_binmat, from=x,to=x))>0)))
                                    han_table <- do.call(rbind,mapply(function(x,grant,legodata) cbind(Grant=grant,legodata[match(x,legodata$Handle),]),han_idx,dt_grant2,MoreArgs=list(legodata=legodata),SIMPLIFY=FALSE))
                                    han_table$LevDistance <- ifelse(han_table$LevDistance>5,5,han_table$LevDistance)/5
                                    han_table$IntersectionalityScore <- han_table$IntersectionalityScore/5 
                                    var_name <- names(han_table)[c(3:7,9)]
                                    plot_data <- han_table[,c("Handle","Grant",var_name)]%>% gather(key = "Metric",value="Value",all_of(var_name),na.rm=TRUE)
                                    plot_data$Metric <- factor(plot_data$Metric,levels=var_name)
                                    
                                    ## Box Plots
                                    plot <- ggplot(plot_data, aes(x=Metric, y=Value, fill=Grant)) +
                                            geom_boxplot(width=.5) + 
                                            scale_fill_manual(values=alpha(brewer.pal(5, "Dark2"),.7)[1:length(dt_grant)])+
                                            theme_fivethirtyeight() +
                                            theme(
                                                    axis.title = element_text(),
                                                    rect=element_blank(),
                                                    panel.grid = element_blank(),
                                                    panel.background= element_blank(),
                                                    plot.background = element_blank()
                                            ) +
                                            watermark_img(filename = "www/gc.png", location = "tr", width = 40, alpha = 0.5) +
                                            ylim(0,1)+
                                            labs(
                                                    subtitle = "Boxplot comparison between the Grant Donor's Sybil Scoring Lego metrics",
                                                    x = "Metric",
                                                    y = "Score"
                                            )
                                    plot
                        }, bg="transparent")

    ## Grant Donor DNA
    gddna <- reactiveValues(nn_nodes=NULL,clusters=NULL,nocluster=FALSE)
    observeEvent(input$grant_ddna_network_cutoff_update,{
                                    gddna$nn_nodes <- NULL
                                    gddna$clusters <- NULL
                                    gddna$nocluster <- FALSE
                                    if(length(input$grant_sel)>0)
                                    {
                                        dt_grant <- input$grant_sel
                                        dt_grant2 <- owner_df$title[match(dt_grant,owner_df$grant_id)]
                                        han_idx <- lapply(match(dt_grant,data_gdna_scores$GrantId),function(x) names(which(unlist(read_fst(data_gdna_binmat, from=x,to=x))>0)))
                                        han_idx_fil <- data.frame(Handle=unique(unlist(han_idx)))
                                        if(nrow(han_idx_fil)>1) 
                                        {   
                                            han_idx_fil$Group = paste0("Donated To ",sapply(apply(sapply(han_idx,function(x,y) y%in% x,y=han_idx_fil$Handle),1,function(x,y) y[x],y=dt_grant),paste0,collapse=", "))
                                            han_idx_fil$GroupSize = apply(sapply(han_idx,function(x,y) y%in% x,y=han_idx_fil$Handle),1,sum)
                                        }
                                        han_idx_fil <- han_idx_fil[data_ddna_scores$Distance_Min[match(han_idx_fil$Handle,data_ddna_scores$Handle)]<=input$grant_ddna_network_cutoff,,drop=FALSE]
                                        if(nrow(han_idx_fil)==0) gddna$nocluster <- TRUE
                                        if(nrow(han_idx_fil)>0)
                                        {
                                            dt_json <- data_ddna_matches_loaded$Handle_Match[match(han_idx_fil$Handle,data_ddna_scores$Handle)]
                                            nn_nodes <- data.frame(
                                                                    name = han_idx_fil$Handle,
                                                                    group = han_idx_fil$Group,
                                                                    groupsize = han_idx_fil$GroupSize,
                                                                    size = 1
                                                                )
                                            nn_links <- do.call(rbind,mapply(grant_ddna_make_links,dt_json,han_idx_fil$Handle,MoreArgs=list(nodes=han_idx_fil$Handle,cutoff=input$grant_ddna_network_cutoff),SIMPLIFY=FALSE,USE.NAMES=FALSE))
                                            network <- graph_from_edgelist(cbind(nn_links$source+1,nn_links$target+1))
                                            network_c <- components(network)
                                            nn_nodes$cluster <- network_c$membership
                                            nn_nodes$clustersize <- network_c$csize[nn_nodes$cluster]
                                            cluster_stats <- data.frame(
                                                                            ClusterId = 1:network_c$no,
                                                                            SelectedGrantsDonated = ceiling(tapply(nn_nodes$groupsize,nn_nodes$cluster,mean)),
                                                                            ClusterSize = network_c$csize,
                                                                            AvgGrantsDonated = ceiling(tapply(legodatafull$DDNAGrantsDonated[match(nn_nodes$name,legodatafull$Handle)],nn_nodes$cluster,mean))
                                                                        )
                                            cluster_stats <- cluster_stats[with(cluster_stats, order(-SelectedGrantsDonated,-ClusterSize,-AvgGrantsDonated)),]
                                            gddna$nn_nodes <- nn_nodes
                                            gddna$clusters <- cluster_stats
                                        }
                                    }
    })
    output$gddna_table <- renderDataTable({
                                            cluster_stats <- gddna$clusters
                                            if(gddna$nocluster) stop("No Cluster Found")
                                            if(is.null(cluster_stats)) return(NULL)
                                            names(cluster_stats) <- sprintf('<div style="font-size:10px;">%s</div>', names(cluster_stats))
                                            datatable(
                                                        cluster_stats,
                                                        escape = FALSE,
                                                        rownames=FALSE,
                                                        options = list(
                                                                        paging = TRUE,
                                                                        bInfo = FALSE,
                                                                        ordering=TRUE,
                                                                        searching=FALSE,
                                                                        autoWidth = TRUE,
                                                                        bLengthChange = FALSE,
                                                                        pageLength = 20
                                                                    )
                                                    )%>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)), border = '1px solid #ddd') %>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)), fontSize = '10px') %>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)),lineHeight='10%') 
                                        })
    # output$gddna_cluster_selUI <- renderUI({
    #                                 if(gddna$nocluster) return(NULL)
    #                                 cluster_stats <- gddna$clusters
    #                                 if(is.null(cluster_stats)) return(NULL)
    #                                 list(selectizeInput("gddna_cluster_sel", label = "Select Cluster (Max 5)", multiple = TRUE, choices = cluster_stats$ClusterId,selected = cluster_stats$ClusterId[1],options = list(maxItems = 5)))
    #                             })
    observeEvent(list(gddna$clusters,gddna$nocluster),{
                                    cluster_stats <- gddna$clusters
                                    if(is.null(cluster_stats)) updateSelectizeInput(session, 'gddna_cluster_sel', choices = NULL, selected=NULL, server = TRUE,options = list(maxItems = 5))
                                    updateSelectizeInput(session, 'gddna_cluster_sel', choices = cluster_stats$ClusterId, selected=cluster_stats$ClusterId[1], server = TRUE,options = list(maxItems = 5))
    })
    output$gddna_network <- renderForceNetwork({
                                                ## If NULL
                                                nn_nodes <- gddna$nn_nodes
                                                if(is.null(nn_nodes)) return(NULL)
                                                if(is.null(input$gddna_cluster_sel)) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_nodes <- nn_nodes[nn_nodes$cluster %in% input$gddna_cluster_sel,]
                                                nn_nodes2 <- data.frame(
                                                                            name = paste0("Cluster_",unique(nn_nodes$cluster)),
                                                                            group = "Cluster",
                                                                            groupsize = 1,
                                                                            size = 9,
                                                                            cluster = unique(nn_nodes$cluster),
                                                                            clustersize = 0
                                                                        )
                                                nn_linksN <- data.frame(
                                                                        source = (1:nrow(nn_nodes))-1,
                                                                        target = nrow(nn_nodes)+match(nn_nodes$cluster,nn_nodes2$cluster)-1,
                                                                        value = 0
                                                                    )
                                                nn_nodesN <- rbind(nn_nodes,nn_nodes2)
                                                forceNetwork(
                                                                Links = nn_linksN,
                                                                Nodes = nn_nodesN,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                legend = FALSE,
                                                                fontSize = 10,
                                                                opacityNoHover = ifelse(nrow(nn_nodesN)<100,1,0),
                                                                bounded=TRUE,
                                                                charge=-10,
                                                                radiusCalculation = "Math.sqrt(d.nodesize)+2",
                                                                zoom=TRUE
                                                            )
                            })


    ########################################################################
    ########################################################################


    ########################################################################
    ## Sybil Clusters
    ########################################################################
    updateSelectizeInput(session, 'sybil_cluster_sel', choices = sybil_clusters$ClusterId, selected=sybil_clusters$ClusterId[1], server = TRUE,options = list(maxItems = 5)) 
    output$sybil_table <- renderDataTable({
                                            cluster_stats <- sybil_clusters
                                            names(cluster_stats) <- sprintf('<div style="font-size:10px;">%s</div>', names(cluster_stats))
                                            datatable(
                                                        cluster_stats,
                                                        escape = FALSE,
                                                        rownames=FALSE,
                                                        options = list(
                                                                        paging = TRUE,
                                                                        bInfo = FALSE,
                                                                        ordering=TRUE,
                                                                        searching=FALSE,
                                                                        autoWidth = TRUE,
                                                                        bLengthChange = FALSE,
                                                                        pageLength = 20
                                                                    )
                                                    )%>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)), border = '1px solid #ddd') %>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)), fontSize = '10px') %>%
                                            formatStyle(columns = c(1:ncol(cluster_stats)),lineHeight='10%') 
                                        })
    output$sybil_network <- renderForceNetwork({
                                                ## If NULL
                                                if(is.null(input$sybil_cluster_sel)) return(NULL)

                                                ## Prepare Nodes and Links
                                                nn_nodes <- sybil_nodes
                                                nn_nodes <- nn_nodes[nn_nodes$cluster %in% input$sybil_cluster_sel,]
                                                nn_nodes2 <- data.frame(
                                                                            name = paste0("Cluster_",unique(nn_nodes$cluster)),
                                                                            group = "Cluster",
                                                                            size = 9,
                                                                            cluster = unique(nn_nodes$cluster),
                                                                            clustersize = 0
                                                                        )
                                                nn_linksN <- data.frame(
                                                                        source = (1:nrow(nn_nodes))-1,
                                                                        target = nrow(nn_nodes)+match(nn_nodes$cluster,nn_nodes2$cluster)-1,
                                                                        value = 0
                                                                    )
                                                nn_nodesN <- rbind(nn_nodes,nn_nodes2)
                                                forceNetwork(
                                                                Links = nn_linksN,
                                                                Nodes = nn_nodesN,
                                                                Source = "source", 
                                                                Target = "target",
                                                                Value = "value", 
                                                                NodeID = "name",
                                                                Nodesize = "size",
                                                                Group = "group",
                                                                opacity = 1,
                                                                legend = FALSE,
                                                                fontSize = 10,
                                                                opacityNoHover = ifelse(nrow(nn_nodesN)<100,1,0),
                                                                bounded=TRUE,
                                                                charge=-10,
                                                                radiusCalculation = "Math.sqrt(d.nodesize)+2",
                                                                zoom=TRUE
                                                            )
                            })
    ########################################################################
    ########################################################################


    ########################################################################
    ## Handle Filter UI
    ########################################################################
    filter_cons <- reactiveValues(filters=list())
    filter_set <- reactiveValues(set=data.frame())
    output$filterUI1 <- renderUI({
                                    list(
                                            selectInput("filter_sel_var", label = NULL, choices = names(legodatafull)[-1], selected = names(legodatafull)[2]),
                                            awesomeRadio("filter_sel_con",NULL, c("Is Equal To"=1,"Less Than or Equal To"=2,"More Than or Equal To"=3,"In Range (Inclusive)"=4,"Outside Range (Exclusive)"=5), inline=FALSE)
                                    )
                                })
    output$filterUI2 <- renderUI({
                                    if(length(input$filter_sel_var)==0) return(NULL)
                                    sel_var_data <- as.list(legodatafull)[[match(input$filter_sel_var,names(legodatafull))]]
                                    sel_var_cutoff <- 1
                                    if(input$filter_sel_con>=4) sel_var_cutoff <- c(min(sel_var_data,na.rm=TRUE),max(sel_var_data,na.rm=TRUE))
                                    list(
                                            sliderInput(
                                                            "filter_sel_cutoff",
                                                            label = NULL,
                                                            min = min(sel_var_data,na.rm=TRUE),
                                                            max = ifelse(max(sel_var_data,na.rm=TRUE)<1,1,max(sel_var_data,na.rm=TRUE)),
                                                            value = sel_var_cutoff,
                                                            step= ifelse(max(sel_var_data,na.rm=TRUE)>1 | length(unique(sel_var_data))==2,1,.05)
                                                        )
                                    )
                                })
    output$filterUI3 <- renderUI({
                                    if(length(input$filter_sel_var)==0) return(NULL)
                                    sel_var_data <- as.list(legodatafull)[[match(input$filter_sel_var,names(legodatafull))]]
                                    miss_data_pct <- round(100*(sum(is.na(sel_var_data))/length(sel_var_data)),2)
                                    list(
                                            h5(paste0(miss_data_pct,"% ",input$filter_sel_var," Missing in Data")),
                                            br(),
                                            awesomeRadio("filter_sel_miss",NULL, c("Include those with Missing Score"=1,"Exclude those with Missing Score"=2), inline=FALSE,selected=2)
                                    )
                                })
    observeEvent(input$filter_sel_add,{
            if(length(input$filter_sel_var)>0)
            {
                filter_curr <- list()
                filter_curr$var = input$filter_sel_var
                filter_curr$con = input$filter_sel_con
                filter_curr$cutoff = input$filter_sel_cutoff
                filter_curr$miss = input$filter_sel_miss
                filter_cons$filters[[length(filter_cons$filters)+1]] <- filter_curr
            }
    })
    observeEvent(input$filter_sel_rem,{
            if(length(filter_cons$filters)>0) filter_cons$filters[[length(filter_cons$filters)]] <- NULL
    })
    output$filterShow <- renderText({
                                        if(length(filter_cons$filters)==0) return(NULL)
                                        paste0(sapply(filter_cons$filters,print_filter),collapse="\nAND\n")
                            })
    output$filter_setsize_1 <- renderValueBox({
                                                # if(length(filter_cons$filters)==0) return(NULL)
                                                valueBox(nrow(legodatafull), "Total handles",width=NULL,color="olive")
                        })
    output$filter_setsize_2 <- renderValueBox({
                                                # if(length(filter_cons$filters)==0) return(NULL)
                                                valueBox(nrow(filter_set$set), "Filtered handles",width=NULL,color="yellow")
                        })
    observeEvent(filter_cons$filters,{
                                if(length(filter_cons$filters)==0) filter_set$set <- legodatafull
                                if(length(filter_cons$filters)>0)
                                {
                                    all_fil_res <- do.call(cbind,lapply(filter_cons$filters,process_filter,data=legodatafull))
                                    filter_set$set <- legodatafull[apply(all_fil_res,1,function(x) all(x)),]
                                }
                        })
    output$filterexp <- renderUI({
                                    if(length(filter_cons$filters)==0) return(NULL)
                                    list(downloadBttn('filterData', 'Download Filtered Dataset',style="simple"))
                                })
    output$filterData <- downloadHandler(
                                            filename = function()
                                            { 
                                                paste0("FilterHandles_",round(as.numeric(Sys.time())),".csv")
                                            },
                                            content = function(con)
                                            {
                                                write.csv(filter_set$set, con)
                                            }
                                )
    ########################################################################
    ########################################################################  
}