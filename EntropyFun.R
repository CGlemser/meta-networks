# Entropy for Networks with given Color Assignment
# now in R :-))

###########################################################
# This program evaluates the entropy of networks with given class assignment (type)
# The input of the program is a square adjacency matrix a of dimension n
# and a vector of dimension n describing the assignment type(i)=q
# size(type)=n,1
# with q integer between 1 and Q identifying Q classes 
###########################################################
# The output of the program is the entropy Sigmac  
# Code part of the Supplementary material: Assessing the relevance of node
#  features for network structure - Ginestra Bianconi, Paolo Pin, and
#  Matteo Marsili1, PNAS, April 2009
#
# If you use this code please cite:
# G. Bianconi, P. Pin, M. Marsili
# Assessing the relevance of node features for network structure
#   Proceedings of the National Academy of Sciences 106, no. 28 (2009):
#   11433-11438.
#
###########################################################

# input:
#   m:    square adjacency matrix of dimension n
#   type: vector of dimension n describing assignment type(i) = q
#   n:    dimension of matrix "m" and vector "type"
#   Q:    number of classes?
# output:
#   Sigmac = entropy
#   W      = ?

EntropyQ <- function(m, type, n, Q){

  # example:
  type <- c(1, 1, 2, 2)
   n <- 4
   Q <- 2
   m <- matrix(c(1, 1, 0, 1,
  				        1, 1, 1, 0,
                0, 1, 1, 1,
                 1, 0, 1, 1), byrow = TRUE, ncol = 4)
  
  precision <- 10^(-3)
  pi   <- 3.14159265358979323846
  undN <- m
  undN <- undN > 0
  und  <- rowSums(undN) # zeilenweise aufsummiert (connections per node)
  avgconn <- sum(und)/n   # average nums of connections
  
  # Compute entropy with given degree sequence and community assignment ---
  
  ## community assignment ##
  A <- matrix(0, ncol = Q, nrow = Q)  # create A matrix
  
  # A(i,j): number of links between elements of class i & j
  # matrix a, dimension Q (Q classes) -> cell: num of connections
  # diag(A) -> number of connections between same class
  
  for(i in 1:Q){
    for(j in 1:Q){
      A[i, j] <- sum(undN[type==i, type==j])
  	}
  }
  
  ## calculations ##
  # compute exp(Lagrangian multipliers)
  z <- runif(n)  # z = exp(omega)
                 # computes vector of length n, drawn from standard normal 
  z <- und/sum(und)  # relative proportion of connections per node
  W <- A/sum(A)  # relative proportion of connections between classes
  
  # W(i,j)=exp( w(type(i),type(j)) )
  
  oldW <- matrix(0, nrow = Q, ncol = Q)
  oldz <- rep(0, n)
  Scold <- 100
  
  for(k in 1:2000){
     bigW <- W[type, type]  # proportion as in adjacency matrix m
     Um <- matrix(rep(z, n), ncol = n, byrow = TRUE) * bigW
   	 # U-Matrix: 4 Reihen, mit jeweils gleichem z-vector
  	 #           elementweise multipliziert mit bigW
  	 # each cell: relative proportion of node connections (z), multiplied
  	 #            by relative proportion of class connections (W)
  	
     Dm <- 1 + (tcrossprod(z) * bigW);
  	 # each cell: 1 + (proportion of node i connection, multiplied by
  	 #                 proportion of node j connections, multiplied by
  	 #                 proportion of class connection
  
     Um <- matrix(sapply(Um, max, 10^(-15)), ncol = n)
     Dm <- matrix(sapply(Dm, max, 10^(-15)), ncol = n)
     # replaces 0 entries with 10^(-15), cause it takes for
  	 # each cell the maximum of cell entry or 10^(-15)
  
     z <- und / rowSums(Um/Dm - diag(diag(Um/Dm)))
  	 # (node connection proportions) / (Um/Dm w/ zero diagonal)
  
     z <- sapply(z, max, 10^(-15))  # replace 0 entries
  	 z <- z * (und > 0)*1  # set isolated z value to zero 
     
  	 for(k in 1:10){
       d <- matrix(0, ncol = Q, nrow = Q);
       bigW <- W[type, type];
       for(i in 1:Q){
         for(j in 1:Q){
           a <- (type==i)+0
  		     b <- (type==j)+0
           M <- (c(a %*% b) * (tcrossprod(z)) / (1 + tcrossprod(z) * W[i,j]))
           
  				 d[i,j]= sum(M - diag(diag(M)))
           if(d[i,j] > 0){
             W[i,j] <- A[i,j] / d[i,j]
  				 } else {
             W[i,j] <- 0;
  				 }
         }    
       }
  	 }
     
    M  <- log(1 + (tcrossprod(z) * W[type, type]))
  
    z  <- z + 0 * (und == 0)
  	mat.temp <- A * log(W + (W==0))
    Sc <- (1/n) * (- sum(log(z + (z==0)) * und) -
                     sum(mat.temp[upper.tri(mat.temp, diag = TRUE)]) + 
  									 sum(M[upper.tri(M)]));
    # Sc = Sc - (1/n)*(0.5*sum(log(2*pi*und + (und==0))) -
    #                 0.5*sum(sum(triu(log(2*pi*A+(A==0))))));
  
    if(k > 150 && max(max(abs(Sc - Scold))) < precision){
      break
    }
    
  	Scold <- Sc
  }
  
  
  M <- log(1 + (tcrossprod(z) * W[type, type]))
  
  z  <- z + 0 * (und == 0)
  mat.temp <- A * log(W + (W==0))
  Sc <- (1/n) * (- sum(log(z + (z==0)) * und) -
                   sum(mat.temp[upper.tri(mat.temp, diag = TRUE)]) + 
  								 sum(M[upper.tri(M)]))
  # Sc = Sc - (1/n)*(0.5*sum(log(2*pi*und + (und==0)))) -
  #          0.5*sum(sum(triu(log(2*pi*A+(A==0))))));
  # too many brackets!?
  out <- list(W = W, Sigmac = Sc)
  out
}

