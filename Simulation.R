sim.net <- function(nauthors, npapers, sdauthors = 1, maxauthors = 10)
{
  twomodenet <- matrix(0, nrow = npapers, ncol = nauthors)
  nauthorsbypaper <- sample(1:maxauthors, npapers, replace = TRUE)
  
  authors <- lapply(nauthorsbypaper, function(i)
    sample(1:nauthors, size = i, replace = FALSE)) 
  
  for(i in seq(npapers))
    twomodenet[i,authors[[i]]] <- 1
  
  authoreffects <- rnorm(nauthors, sd = sdauthors)
  originaleffects <- rnorm(npapers)
  
  finaleffects <- twomodenet %*% authoreffects + originaleffects
  
  meta.analysis.dataset <- data.frame(cbind(finaleffects, twomodenet))
  names(meta.analysis.dataset) <- c("index", paste0("author", seq(nauthors)))
  row.names(meta.analysis.dataset) <- paste0("paper", seq(npapers))
  meta.analysis.dataset
}


ma <- sim.net(nauthors = 100, npapers = 100, sdauthors = 0)
tmd <- as.matrix(ma[,-1])
papernet <- tmd %*% t(tmd)
diag(papernet) <- 0
stat <- ma[,1]
vsz <- 10*(stat-min(stat))/max(stat-min(stat))
qg.papernet <- qgraph(papernet, layout = "spring", vsize = vsz)
ig.papernet <- as.igraph(qg.papernet)
covQAPtest(ig.papernet, stat, numreps = 500, conf.level = .95)



