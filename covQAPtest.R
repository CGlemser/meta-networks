########### QAP test ##############

# stat: mean(cor(ES, mean(ES in neighbourhood)))
# -> permutations:
#    - permute effect sizes across nodes (structure stays the same)
#    - recalculate stat: mean correlation of ES w/ ES in neighbourhood
#    - construct distribution of the stat
# -> compare original stat with permutation distribution of stat

# Input: adjacency matrix, effect size vector, number of permutations
#        & confidence level
#
# Output: observed correlation, vector of permutation correlations,
#         confidence interval, number of used observations (in case of NAs)

covQAPtest <- function(net.ig, stat, numreps = 500, conf.level = .95){
  numV <- vcount(net.ig)
  neigh <- ego(net.ig)

  statNeigh <- numeric(vcount(net.ig))
  
  for(i in 1:numV){
    hood <- neigh[[i]]
    
    # length(hood) == 1 means node has no neighbours
    if(length(hood) == 1){
  	  statNeigh[i] <- 0
  	} else {
  		# if effect size is missing for the ego node, it is not considered
  	  if(is.na(stat[i])){
    	  statNeigh[i] <- NA
  	  # otherwise: take the mean effect size of all neighboring nodes
  		} else {
  			statNeigh[i] <- mean(stat[hood[-1]])  # first node is ego node
  		}
  	}
  }
	# observed correlation between node statistic and statistics in
	#   neighbourhood (calculated only for complete observations)
  cor.ori <- cor(stat, statNeigh, use = "pairwise.complete.obs")
  
  cor.per <- numeric(numreps)
	used.obs <- numeric(numreps)
  for(j in 1:numreps){
    stat.per <- sample(stat)
    statNeigh.per <- numeric(numV)
    for(i in 1:numV){
      hood <- neigh[[i]]

      # length(hood) == 1 means node has no neighbours
      if(length(hood) == 1){
    	  statNeigh.per[i] <- 0
    	} else {
    		# if effect size is missing for the ego node, it is not considered
    	  if(is.na(stat.per[i])){
    		  statNeigh.per[i] <- NA
    	  # otherwise: take the mean effect size of all neighboring nodes
    		} else {
    			statNeigh.per[i] <- mean(stat.per[hood[-1]],
																	 na.rm = TRUE)
    		}
    	}
    }
    cor.per[j] <- cor(stat.per, statNeigh.per, use = "pairwise.complete.obs")
		used.obs[j] <- dim(na.omit(cbind(stat.per, statNeigh.per)))[1]
  }

	alph.err <- 1 - conf.level

	CI <- quantile(cor.per, c(alph.err/2, 1 - alph.err/2))	
	rejectH0 <- unname(cor.ori < CI[1] | cor.ori > CI[2])

  plot(density(cor.per))
  # .025/.975 quantiles -> cutoff points for rejecting H0
  abline(v = CI, lty = 2, col = "blue")
  abline(v = cor.ori, col = "red")  # observed correlation

	output <- list(cor.ori = cor.ori, cor.per = cor.per, CI = CI,
								 rejectH0 = rejectH0, conf.level = conf.level,
								 used.obs = used.obs)

	class(output) <- "covQAPtest"
	output
}

## print function for covQAP function ##
print.covQAPtest <- function(obj){
  cat("\nQuadratic Assignment Procedure (QAP) for node attributes")
  cat("\n\nTests for significance of the correlation of the ego's attribute",
			"\nlevel with the attribute level of its neighbouring nodes")
	cat("\n\nObserved correlation:", round(obj$cor.ori, 3))
	cat("\n", sprintf("%1.0f%%", obj$conf.level*100), "-CI: [",
			round(obj$CI[1], 3), "; ", round(obj$CI[2], 3), "]", sep = "")
  cat("\n\nThe ego and neighbouring attribute levels are significantly",
			"\ncorrelated:", obj$rejectH0)
	cat("\n\nMean used observations:", round(mean(obj$used.obs), 3), "\n")
}

## QUESTIONS
# - simulation to see whether test would detect actual biases 
# - when shuffling the data: what to do with NA values?
#   -> include in shuffling or keep fixed and only shuffle available values?
# - how many reps/permutations are enough?
