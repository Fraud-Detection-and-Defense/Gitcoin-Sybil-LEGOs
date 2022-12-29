## Loading Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(omnitheme)
options(scipen=999)

## Load whole data
legodata <- read_csv("~/Desktop/Gitcoin Sybil LEGOs/data_app/LegoData.csv")

## Lev Plot
y_lim1 <- c(0,15000)
p1 <-   legodata %>%
		group_by(LevDistance) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = LevDistance, y = count)) +
		annotate("rect", xmin = -Inf, xmax = 1.5, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		annotate("rect", xmin = 1.5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		geom_bar(stat = "identity", fill = "#6F3FF5", width=.35) +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim1) + 
		scale_x_continuous(breaks = (1:5),limits=c(0,6)) + 
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of the Minimum Levenshtein Distances across Gitcoin Account Names.",
				x = "Minimum Levenshtein Distance",
				y = "Number of Accounts"
		)
saveRDS(p1,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_Lev.RDS")
# ggsave("~/Plot_Lev.jpg",plot=p1,width = 12,height=6)

## Intersectionality
y_lim2 <- c(0,12000)
p2 <-   legodata %>%
		group_by(IntersectionalityScore) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = IntersectionalityScore, y = count)) +
		annotate("rect", xmin = -Inf, xmax = .5, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		annotate("rect", xmin = .5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		geom_bar(stat = "identity", fill = "#6F3FF5",width=.5) +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim2) + 
		scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of the On-Chain Intersectionality Scores",
				x = "On-chain Intersectionality Score",
				y = "Number of Accounts"
		)
saveRDS(p2,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_Intersectionality.RDS")
# ggsave("~/Plot_Intersectionality.jpg",plot=p2,width = 12,height=6)

## DDNA
y_lim3 <- c(0,22000)
p3 <-   legodata %>%
		group_by(DDNADistance) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = DDNADistance, y = count)) +
		annotate("rect", xmin = -Inf, xmax = .1, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		annotate("rect", xmin = .5, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		geom_bar(stat = "identity", fill = "#6F3FF5") +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim3) + 
		scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of the Minimum Jaccard Distances across Gitcoin Donor DNA Profiles.",
				# subtitle = "DonorDNA profiles have been derived from both the collection of sets and the total amount donated per user.",
				x = "Jaccard Minumum Distance between Donors",
				y = "Number of Accounts"
		)
saveRDS(p3,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_DDNA.RDS")
# ggsave("~/Plot_DDNA.jpg",plot=p3,width = 12,height=6)

## APU
y_lim4 <- c(0,4000)
p4 <-   legodata %>%
		group_by(APUScore) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = APUScore, y = count)) +
		annotate("rect", xmin = -Inf, xmax = .1, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		annotate("rect", xmin = .2, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		geom_bar(stat = "identity", fill = "#6F3FF5") +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim4) + 
		scale_x_continuous(breaks = (1:10)/10,limits=c(0,1)) +
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of APU Scores for Passport Users.",
				x = "APU Score",
				y = "Number of Accounts"
		)
saveRDS(p4,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_APU.RDS")
# ggsave("~/Plot_APU.jpg",plot=p4,width = 12,height=6)

## SAD
y_lim5 <- c(0,30000)
p5 <-   legodata %>%
		group_by(SADScore) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = SADScore, y = count)) +
		annotate("rect", xmin = -Inf, xmax = .4, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		annotate("rect", xmin = .7, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		geom_bar(stat = "identity", fill = "#6F3FF5") +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim5) + 
		scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of SAD Prediction Scores for Gitcoin Accounts.",
				x = "SAD Prediction Score",
				y = "Number of Accounts"
		)
saveRDS(p5,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_SAD.RDS")
# ggsave("~/Plot_SAD.jpg",plot=p5,width = 12,height=6)

## IPs
y_lim6 <- c(0,18000)
p6 <-   legodata %>%
		group_by(IPsShared) %>%
		summarise(count = n()) %>%
		ggplot(aes(x = IPsShared, y = count)) +
		annotate("rect", xmin = -Inf, xmax = .4, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "green") +
		annotate("rect", xmin = .7, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.25, fill = "red") +
		geom_bar(stat = "identity", fill = "#6F3FF5") +
		scale_y_continuous(breaks = scales::pretty_breaks(n = 10),limits=y_lim6) + 
		scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
		theme_fivethirtyeight() +
		theme(
				axis.title = element_text(),
				rect=element_blank(),
				panel.grid = element_blank(),
				panel.background= element_blank(),
				plot.background = element_blank()
		) +
		watermark_img(filename = "~/Desktop/Gitcoin Sybil LEGOs/www/gc.png", location = "tr", width = 40, alpha = 0.5) +
		labs(
				# title = "Distribution of ip_ratio_gt5 for Gitcoin Accounts.",
				x = "Ratio Shared IPs",
				y = "Number of Accounts"
		)
saveRDS(p6,"~/Desktop/Gitcoin Sybil LEGOs/data_app/Plot_IPs.RDS")
# ggsave("~/Plot_IPs.jpg",plot=p6,width = 12,height=6)

