# Theta code in R

# This program evaluates the indicator Theta of networks with 
# given class assignment (type)
# The input of the program is a square adjacency matrix a of 
# dimension n and a vector of dimension n describing the 
# assignment type(i)=q size(type)=n,1
# with q integer between 1 and Q identifying Q classes 
# Nrun_max is the number of random permutation used to calculate Theta
######################################################################
# The output of the program is the indicator Theta and the matrix W
# Code part of the Supplementary material: Assessing the relevance of 
# node features for network structure
# Ginestra Bianconi, Paolo Pin, and Matteo Marsili1, PNAS, April 2009
#
# This code can be redistributed and/or modified
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#  
# This program is distributed ny the authors in the hope that it will be 
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#  
# If you use this code please cite 
#
# [1] G. Bianconi, P. Pin, M. Marsili
# Assessing the relevance of node features for network structure
# Proceedings of the National Academy of Sciences 106, no. 28 (2009): 11433-11438.
#
######################################################################



Theta <- function(m, type, n, Q, Nrunmax){

  EntropyOut <- EntropyQ(m, type, n, Q)
  
  entropymean <- 0
  entropy2 <- 0
  
  for(nrun in 1:Nrunmax){
    nrun
    x <- sample(1:n)
    type2 <- type[x]
     
    out <- EntropyQ(m, type2, n, Q)
		Sigmac <- out$Sigmac
   
    entropymean <- entropymean + Sigmac/Nrunmax  # E(Entropy)
    entropy2 <- entropy2 + Sigmac*Sigmac/Nrunmax
	}
  
  entropy2 <- entropy2 - entropymean^2  
  deltaentropy <- sqrt(entropy2)        # SD(entropy)
  theta <- (EntropyOut$Sigmac - entropymean)/deltaentropy
  
  CI <- c(entropymean + qnorm(.975)*deltaentropy,
					entropymean - qnorm(.975)*deltaentropy)
	

  output <- list(theta = theta, Sigmac = EntropyOut$Sigmac,
								 W = EntropyOut$W, EEntropy = entropymean,
								 SDentropy = deltaentropy, CI = CI)
  output
}

