# Testrun.R
# 
# Input: fulldata_noetal.txt
#        extractAuthorsFun.R
#        statTransformFun.R
#        plotNetworkFun.R
#        addLegend.R
#        EntropyFun.R
#        ThetaFun.R
# 
# Output: Testplot_Legend.pdf
#         Testplot_noLegend.pdf
#
# Claudia Glemser, 26/Jan/18

# tests the functions implemented so far with an example dataset:
#   extractAuthors
#   plotNetwork (embedded: statTransform)
#   addLegend

# set working directory
setwd("F:/GoogleDrive/Mailand_Praktikum/MetaAnalysisNetworks/meta-networks")

#######################
## testing out plots ##
#######################

# source functions
source("extractAuthorsFun.R")
source("statTransformFun.R")
source("plotNetworkFun.R")
source("addLegend.R")

library(plotrix) 
library(qgraph)
library(igraph)

# load data
dat <- read.table("fulldata_noetal.txt")
dat <- dat[order(as.character(dat$Reference)),]

RefList <- extractAuthors("Reference", year = TRUE, dat, minPubs = 0)
datRef <- RefList[[1]]
authors <- RefList[[2]]
datRef <- datRef[order(as.character(datRef$ref)),]

datRef$stat <- dat$Effect_Size_Cohens_d
datRef$year <- dat$Publication_Year

testplot  <- plotNetwork("ref", datRef, c("deeppink", "lightskyblue"),
												 minPubs = 2, color = "stat", size = "year",
												 auts = authors, legendSpace = TRUE)
addLegend(testplot, datRef, "year", "stat")

testplot2 <- plotNetwork("ref", datRef, c("chocolate", "olivedrab"), 1,
												 color = "year", size = "stat", auts = authors,
												 legendSpace = FALSE)

testplot3 <- plotNetwork("ref", datRef, c("chocolate", "olivedrab"),
												 color = "year", size = "stat", auts = authors,
												 paperNet = TRUE, size.scaling = c(.5, 1.5))
addLegend(testplot3, datRef, size.var = "stat", color.var = "year",
					cex.legend = .8, cex.title = .8,
					title = c("effect size", "publication year"))
# when paperNet=TRUE, error during igraph transformation -> figure out why

testplot4 <- plotNetwork("ref", datRef, c("chocolate", "olivedrab"), 1,
												 color = "year", size = "stat", auts = authors)
pdf("Testplot_Legend.pdf", width = 9, height = 7)
addLegend(testplot4, datRef, size.var = "stat",
					color.var = "year")
dev.off()

## OR W/O LEGEND
pdf("Testplot_noLegend.pdf")
plot(testplot2$net)
dev.off()



###############################
## testing out Entropy stuff ##
###############################
source("EntropyFun.R")
source("ThetaFun.R")

##### !!!!!!!! #######
# extremely slow atm #
##### !!!!!!!! #######

# read out adjacency matrix
library(igraph)
net.ig <- igraph::as.igraph(testplot$net)
adjmat <- as.matrix(igraph::get.adjacency(net.ig))

# cut into 4 classes [0; .2], [.2; .5], [.5; .8] [.8; maxvalue]
catES <- cut(abs(testplot$col_avstat),
		         breaks = c(0, .2, .5, .8, max(abs(testplot$col_avstat),
																					 na.rm = TRUE)),
						 right = FALSE, include.lowest = TRUE)

adjmatES <- adjmat[!is.na(catES), !is.na(catES)]  # delete NA cases
type <- as.numeric(na.omit(catES)) # class vector w/o NA cases
n <- dim(adjmatES)[1]  # no of nodes
Q <- 4  # no of classes

# EntropyQ -> lots of matrix multiplications, which R is apparently bad at
EntropyQ(adjmatES, type, n Q)    # ~25s
Theta(adjmatES, type, n, Q, 10)  # ~300s

# Open questions:
# -> negative entropies??
# -> in Theta.m: shouldn't it be (entropymean - Sigmac)?
#                Or makes no diff? absolute values?
# -> in Theta.m: Signmac at the end is the observed entropy, not the
#                entropy from the last iteration right?
# -> How do I construct CI (SE?) / conduct a test?



########################
## QAP covariate test ##
########################
library(igraph)
source("covQAPtest.R")  # test function + printing function

# transform into igraph object (for some reason 205 nodes instead of 207?)
# also gives out a few warnings?
net.ig <- as.igraph(testplot3$net)
out  <- covQAPtest(net.ig, runif(205, 0, 2), 100, .95)
# for testing purposes: random stats vector (no NAs)

stattest <- testplot3$node_avstat[1:205]
out2 <- covQAPtest(net.ig, stattest, 100, .99)  # lots of NAs
# -> maybe include number of observations that were used for 


###################################
## components (still hard-coded) ##
###################################
# -> make into function once it's fail-safe

# transform to igraph object
net.ig <- as.igraph(testplot$net)
plot(net.ig, layout = layout_with_kk)

# draw components from network & group them
comps      <- groups(igraph::components(net.ig))
comp_sizes <- unlist(lapply(comps, length))

# only plot components with at least 5 nodes (variable to be)
groups_to_plot <- comps[comp_sizes >= 5]

# we need an index.mat to read out the correct columns from original df
index.mat <- matrix(FALSE, nrow = length(groups_to_plot),
										ncol = length(V(net.ig)))
# every row = index vector per group for node labels

# for loop creates 3 lists: author names, data.frame & network per component
node.names <- list(0)
dats.comps <- list(0)
nets.comps <- list(0)

for(i in 1:dim(index.mat)[1]){
	# logical vector for each component to index node labels
  index.mat[i,] <- testplot$net$graphAttributes$Nodes$labels %in%
					           V(net.ig)$label[groups_to_plot[[i]]]
	
	# read out author names from the node labels
  node.names[[i]] <- names(testplot$net$graphAttributes$Nodes$labels)[
										   index.mat[i,]]

	# create new data.frames contaning only the authors of each component
	dats.comps[[i]] <- datRef[,colnames(datRef) %in%
														 c(node.names[[i]], "ref", "stat", "year")]

	# create new networks, each representing one component
	nets.comps[[i]] <- plotNetwork("ref", dats.comps[[i]],
																 auts = node.names[[i]],
																 color = "year", size = "stat") 
}

# example plot of a component
plot(nets.comps[[4]]$net)
addLegend(nets.comps[[4]], dats.comps[[4]],
					size.var = "stat", color.var = "year")

# warning messages if there's no effect sizes for the authors,
# makes all nodes into rectangles, even when one author has an average
#  effect size, but all the others don't
# include rectangle = NA into the legend
