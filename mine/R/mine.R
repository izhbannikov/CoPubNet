process_text <- function(text_to_parse, keywords) {
  #================Loading data==========================#
  #data <- read.csv(filename,header=T)
  #======Taxa==========
  #X <- as.character(lapply(seq_len(dim(data)[1]), function(i) toString(data[i,1])))
  #======Partitions of data set======================
  #ans <- data.frame(data[,2:length(data)])
  #S <- lapply(seq_len(ncol(ans)), function(i) ans[,i])
  #S <- lapply( seq_len(length(S)), function(i) lapply( seq_len(length(S[[i]])), function(j) if ( S[[i]][j] == 1 ) { S[[i]][j] <- X[j] } else { S[[i]][j] <- "NA"  } ) )
  #S <- lapply( seq_len(length(S)), function(i) S[[i]] <- S[[i]][which(S[[i]] != "NA")]  )
  #S <- lapply( seq_len(length(S)), function(i) unlist(S[[i]]))
  #============Functions to compute decisiveness and fix the data set==================
  
  #print(text_to_parse)
  ans <- .Call("process_text",text_to_parse,keywords)
  
  
  
  #============Return value: fixed or not fixed matrix==============
  #final <- matrix(0,ncol=length(ans)-1,nrow=length(X))
  #rownames(final) <- c(levels(data[,1]))
  #colnames(final) <- colnames(data)[2:length(colnames(data))]
  #final<-as.table(final)
  #for(i in seq_len(length(ans)-1)) {
  #  for(j in seq_len(length(ans[[i]]))) {final[ ans[[i]][j], colnames(final)[i] ] <- 1}
  #}
  #out <- list(final,ans[[length(ans)]])
}
