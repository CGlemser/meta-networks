# plotNetworkFun.R
#
# Claudia Glemser, 26/Jan/18

# needs function statTransform!
# needs package plotrix (for legend)

# input:
#   data     = datRef (data.frame of refs, coding author contributions)
#             (cols = studyID+authors, rows = references, cells = 0/1)
#   refsID   = factor column of data, containing the reference/studyID
#   colors   = char vector, color continuum from x to y
#   minPubs  = only include authors with >= minPubs publications
#   color    = statistic to be displayed as node colour
#   size     = statistic to be displayed as node size
#   legend.c = should a colour legend be plotted? 
#   legend.s = should a size legend be plotted?
#   paperNet = plot a network of papers rather than authors
#  size.scaling = how do you want size to scale?
#                 default: .5 - 1 to avoid overlapping
#                 recommended to change for large networks   
#
# output:
#   $net:     qgraph of authors or papers
#   $colgrad: colour legend


if(!require("pacman")) install.packages("pacman") # load some packages
pacman::p_load("dplyr", "qgraph", "igraph")
select <- dplyr::select

plotNetwork <- function(refsID, data, colors = c("lightblue", "darkblue"),
												minPubs = 1, color = NULL, size = NULL,
												auts, legendSpace = TRUE,
												paperNet = FALSE, size.scaling = c(.5, 1)){
	if(size.scaling[2] - size.scaling[1] <= 0){
	  stop("the first value of size.scaling should be smaller than the second")
	}
	# keep only 1 line per reference
  data_short <- data[!duplicated(data[, refsID]),]
  # exclude columns refsID & stat
  twomodenet <- data_short[,colnames(data_short) %in% auts] %>% as.matrix()
	# exclude authors with less than minPubs publications
	if(!paperNet){
	  twomodenet <- twomodenet[,apply(twomodenet, 2, sum) >= minPubs]
	}
	auts_minPubs <- colnames(twomodenet)
	rownames(twomodenet) <- unique(data[, refsID])

	# creates a network of authors
  if(!paperNet){
	  net <- t(twomodenet)%*%twomodenet
	  no_pubs_PA <- diag(net)
	} else {
		net <- twomodenet %*% t(twomodenet)
	  no_auts_PP <- diag(net)
	}
	# set diagonal to zero for plotting
	diag(net) <- 0
 
  # use igraph to visualize the network with colors
  Network <- qgraph(net, layout = "spring", edge.color = "black",
	                  DoNotPlot = TRUE)
	dev.off()

	node_avstat <- col_avstat <- NULL

  # restrict averagestat to 0-100 (node color) / a scalar (node size)
	if(!is.null(size)){
	
  	if(!paperNet){
	    out <- statTransform(size, refsID, data, auts = auts_minPubs, 
															   size = TRUE, byaut = TRUE,
																 size.scaling = size.scaling)
		} else {
      out <- statTransform(size, refsID, data, auts = auts_minPubs,
																 size = TRUE, bypap = TRUE,
																 size.scaling = size.scaling)
		}

	  node_avstat <- out[["avstat"]]
		node_size <- out[["node_size"]]

	  Network$graphAttributes$Nodes$shape[is.na(node_size)] <- "rectangle"
		node_size[is.na(node_size)] <- mean(size.scaling)
	  Network$graphAttributes$Nodes$width <-
		  Network$graphAttributes$Nodes$height <-
			  node_size*Network$graphAttributes$Nodes$height
	}


	if(!is.null(color)){
		if(!paperNet){
	    out <- statTransform(color, refsID, data, auts_minPubs,
	    	                   byaut = TRUE, color = TRUE, colors = colors)
		} else {
		  out <- statTransform(color, refsID, data, auts_minPubs,
	    	                   bypap = TRUE, color = TRUE, colors = colors)
		}

	  col_avstat <- out[["avstat"]]
	  clrs <- out[["clrs"]]
 	  colgrad <- out[["colgrad"]]
	  Network$graphAttributes$Nodes$color <- clrs
	} else {
	  colgrad <- rep("#FFFFFFFF", 100)
	}
  
	if(legendSpace){
	  Network$plotOptions$mar[4] <- 1 
  }

	# include igraph object, adjacency matrix & edge list in output
	# net.ig <- as.igraph(Network)
	# adjM   <- as_adjacency_matrix(net.ig)
	# edgL   <- cbind(Network$Edgelist$from, Network$Edgelist$to,
	#								  Network$Edgelist$weight)

	output <- list(net = Network, colgrad = colgrad,
								 col_avstat = col_avstat, node_avstat = node_avstat)
	# net.ig = net.ig, adjM = adjM, edgL = edgL)

}
