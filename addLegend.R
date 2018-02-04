# addLegend.R
#
# requires plotrix (for gradient.rect)
#
# plots the Network with an added legend
#   net.obj    = output of plotNetwork()
#   size.var   = variable displayed by node size (char)
#   color.var  = variable displayed through colour (char)
#   cex.legend = font size of legend text (num, default: .9)
#   cex.title  = font size of legend title (num, default: 1)
#   title      = character vec of the titles to be displayed
#                (default: size.var, color.var)

addLegend <- function(net.obj, data, size.var = NULL,
											color.var = NULL, cex.legend = .9, cex.title = 1,
											title = c(size.var, color.var)){
	
	Network <- net.obj$net

  if(!is.null(size.var)){
	  sizes <- Network$graphAttributes$Nodes$height
    step.size <- diff(range(sizes))/4
    breaks.size <- min(sizes) + 0:4*step.size
    mean.sizes <- apply(rbind(breaks.size[1:4], breaks.size[2:5]),
  										  2, mean, na.rm = TRUE)

    # calculate respective statistics values
		step.stat <- diff(range(data[, size.var], na.rm = TRUE))/4
    breaks.stat <- min(data[, size.var], na.rm = TRUE) + 0:4*step.stat
    
		# ...and display ranges as labels 
		if(is.integer(data[, size.var])) {
		  stats.size <- round(breaks.stat)
	  } else {
		  stats.size <- round(breaks.stat, 2)
		}

		label.size <- character(4)
		for(i in 1:4){
		  label.size[i]<- paste0("[", stats.size[i], "; ", stats.size[i+1], "]")
		}

    plot(Network)

    # main title above legend
		text(1.3, 1.1, title[1], pos = 4, font = 2, cex = cex.title) 

		# add legend to the plot
		points(x = rep(1.3, 4), y = c(1, .8, .55, .3), pch = 1, cex = mean.sizes)
    text(x = rep(1.4, 4), y = c(1, .8, .55, .3), labels = label.size,
  		   pos = 4, cex = cex.legend)
	}

	if(!is.null(color.var)){
	  if(is.null(size.var)) plot(Network)
	  text(1.3, -.2, title[2], pos = 4, font = 2, cex = cex.title)
	  gradient.rect(1.3, -1, 1.4, -.3, col = net.obj$colgrad,
	   						 nslices = length(colgrad), gradient = "y")
	  text(1.5, -1 ,  round(min(data[, color.var], na.rm = TRUE), 2),
				 cex = cex.legend)
	  text(1.5, -.3, round(max(data[, color.var], na.rm = TRUE), 2),
				 cex = cex.legend)
	}
}

