# statTransform.R
# 
# Claudia Glemser, 16/01/18

# input:
#  x = numeric vector of the statistic to transform
#  refsID = reference char column in data
#  data = data.frame
#  auts = author char vector
#  resol = how fine-grained is color/size resolution?
#  color/size = TRUE/FALSE (statistic displayed as node size or colour?
#  colors = start and end colour (char vector with 2 elements)
#  byaut = aggregate across authors
#  bypap = aggregate across papers
#  size.scaling = how do you want size to scale?
#                 default: .5 - 1 to avoid overlapping
#                 recommended to change for large networks

# output: clrs / node_size
#  vector (length = no. of authors)
#  can then be used as input for qgraph


statTransform <- function(x, refsID, data, auts = NULL, resol = 100,
													color = !size, size = !color,
													colors = c("darkblue", "lightblue"),
													size.scaling = c(.5, 1),
													byaut = !bypap, bypap = !byaut){

	if(size && color) stop("Please pick either size or color to be true!")
  if(byaut && bypap) stop("can only aggregate across authors OR papers!")
	
	# remove refs with no indicated effect size
  statistic <- na.omit(data[, x])
  if(byaut){
	  pairmat <- na.omit(data[, colnames(data) %in% c(auts, x)])
	  pairmat <- pairmat[, colnames(pairmat) != x]  # only authors matrix
		# only include authors with at least one indicated statistic
    # pairmat <- pairmat[,colSums(pairmat) != 0]
    # compute average statistic
    averagestat <- (t(pairmat) %*% statistic)/colSums(as.matrix(pairmat),
																											na.rm = TRUE)
		averagestat <- data.frame(value = row.names(averagestat),
															avstat = averagestat[,1], row.names = NULL)
    valNA <- averagestat$value[is.na(averagestat$avstat)] # auts w/ no info
	}

	if(bypap){
	  averagestat <- aggregate(data[,x] ~ data[,refsID], FUN = mean,
														 na.action = "na.pass")
	  colnames(averagestat) <- c("value", "avstat")  
		# read out paper with NA
	  valNA <- averagestat[is.na(averagestat$avstat), "value"]	
	}
	

	# which authors have only NAs
	statsNA <- data.frame(value  = valNA,
												avstat = rep(-99, length(valNA)))
	stats_full <- rbind(statsNA, na.omit(averagestat))
	stats_full <- stats_full[order(as.character(stats_full$value)),]

  step <- (max(averagestat$avstat, na.rm = TRUE) -
					 min(averagestat$avstat, na.rm = TRUE))/(resol - 1)

	# do this with all authors to match number of nodes in final network!
	
  clrs <- round((stats_full$avstat -
	  					   min(averagestat$avstat, na.rm = TRUE))/step)+1
	clrs[stats_full$avstat == -99] <- NA
  # between 1-100

	output1 <- output2 <- pltt <- 0

	if(color){
		colfunc <- colorRampPalette(colors)
    pltt <- colfunc(resol)
	  clrs <- pltt[clrs]  # assigns correct colour to every author
    clrs[is.na(clrs)] <- "#FFFFFF"  # NAs are black (NAs should be gone)
		output1 <- clrs
  }

	if(size){
	  node_size <- seq(size.scaling[1], size.scaling[2], length.out = 100)[clrs]
#		node_size[is.na(node_size)] <- 1  # maybe squares instead of circles?
		                                   # to make it recognizable
		output2 <- node_size # scales the automatic node height/width of qplot
	}
	list(clrs = output1, node_size = output2, colgrad = pltt,
			 avstat = averagestat$avstat)
}
