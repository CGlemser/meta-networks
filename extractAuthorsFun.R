# extractAuthorsFun.R
# 
# Claudia Glemser, 16/Jan/2018

# input:
#   refs = reference vector
#   year = TRUE (is a year indicated; needs to be after the authors)
#   data = data.frame containing references
#   minPubs = only authors with at least x references are included
# output: data.frame of all references with 0-1 coding of the contributin
#         author (datRef)

if(!require("pacman")) install.packages("pacman") # load some packages
pacman::p_load("dplyr", "qgraph", "igraph")
select <- dplyr::select


extractAuthors <- function(refs, year = TRUE, data, minPubs = 0){
	 ## error message when "et al" detected
#  if(length(grep("et al", data[,refs]))){
#	  stop("Please don't use et al. in your references!\n  ",
#				 "Include all authors and then rerun the function")
#	}
	if(year){
    # the year and everything after it is deleted
    clear_refs <- gsub("\\s*\\S(\\d)+.+", "", data[,refs])
  }

  ## extraction of author surnames ##
  # strsplit()
  # splits at any sequence of ,&space NOT followed by space a-z and dot
  authorList <- sapply(clear_refs, strsplit,
	  									 split = "\\s*(,|&|;)(?!\\s\\w\\.)(\\s|&|;|)*",
		  								 perl = TRUE)
	authors <- unique(unlist(authorList))
	# there can be a/several space(s) at the beginning,
	# needs to be at least one (,|&|;)
	# then there can be one/several spaces. &, ;, or ,
	# takes care of stuff like ", &" without separating "van der ..."

	# create data.frame for coding authors per reference
	datRef <- data.frame(ref = data[,refs])
	nExp <- dim(datRef)[1]   # number of lines = number of experiments
	datRef$row_no <- 1:nExp  # introduce identifier per each row/experiment 
	# create column for each author and code 0-1 
  for(exp_no in 1:nExp){
  # read out the authors for ith experiment
	  aut_temp <- authorList[[exp_no]]
  
    # classify in authors with an already existing column vs new ones 
    new_aut <- aut_temp[!aut_temp %in% colnames(datRef)]
	  exi_aut <- aut_temp[!aut_temp %in% new_aut]

	  # introduce new authors as columns and code them as 1 in relevant line
	  datRef[new_aut] <- ifelse(datRef$row_no == exp_no, 1, 0)
	  if(!identical(exi_aut, character(0))){
	    for(author_no in 1:length(exi_aut)){
	      datRef[datRef$row_no == exp_no, exi_aut[author_no]] <- 1
	    }
	  }
  }
	datRef$row_no <- NULL
  list(codingMat = datRef, authors = authors)  # output
}
