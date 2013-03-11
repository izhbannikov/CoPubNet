library(rentrez)

mine <- function(search_string,search_limit,kterms) {
  #pubmed_search <- entrez_search(db = "pubmed", term = "ph[and]vagina[and]bacteria",retmax=5)
  pubmed_search <- entrez_search(db = "pubmed", term = search_string,retmax = search_limit)
  
  summaries <- entrez_summary(db = "pubmed", ids = pubmed_search$ids)
  authors <- unique(xpathSApply(summaries, "//Item[@Name='Author']",xmlValue))
  titles <- xpathSApply(summaries, "//Item[@Name='Title']", xmlValue)
  #print(summaries)
  
  abstracts <- entrez_fetch(db = "pubmed", ids = pubmed_search$ids,file_format='abstract',retmode='text')
  abstracts <- c(strsplit(abstracts, "\n\n\n"))
  
  key_terms <- c(strsplit("bacterial vaginosis, yeast infection, preterm birth",','))
  
  trial <- matrix(0,ncol=length(authors),nrow=length(authors))
  colnames(trial) <- c(authors)
  rownames(trial) <- c(authors)
  trial.table <- as.table(trial)
  
  #Pie chart
  freq <- matrix(0,ncol=length(authors),nrow=length(kterms)+1)
  colnames(freq) <- c(authors)
  rownames(freq) <- c(kterms,"other")
  freq.table <- as.table(freq)
  
  relevant_authors <- c()
  authors2 <- c(xpathSApply(summaries, "//Item[@Name='AuthorList']"))
  #print(authors2)
  for(k in seq(1, length(authors2)) ) {
    if(length(xmlToList(authors2[[k]]))-1 > 1) {
      b <- sapply(1:(length(xmlToList(authors2[[k]]))-1), function(i) {xmlToList(authors2[[k]])[i]$Item$text} )
      
      if( (b[1] %in% authors) & (tail(b,1) %in% authors) ) {relevant_authors <- unique(c(relevant_authors, b[1],tail(b,1)))}
      
      comb <- combn(b, 2)
      for(i in seq(1:dim(comb)[2])) {
        if ( (comb[,i][1] %in% authors) & (comb[,i][2] %in% authors) ) {
          trial.table[ comb[,i][1], comb[,i][2] ] <- trial.table[ comb[,i][1], comb[,i][2] ] + 1
        } 
      }
      
      for( term in kterms ) {
        cnt <- 0
        if( length(grep(term, titles[k], ignore.case = T)) > 0 ) {
          cnt <- 1
          for(bb in b) {
            if ( bb %in% authors ) {freq.table[term,bb] <- freq.table[term,bb] + 1}
          }
        } 
        #Look for term in an abstract
        if( length(grep(term, abstracts[[1]][k], ignore.case = T)) > 0 ) {
          cnt <- 0.5
          for(bb in b) {
            if ( bb %in% authors ) {freq.table[term,bb] <- freq.table[term,bb] + 0.5}
          }
        }
        
        if(cnt == 0) {
          for(bb in b) {
            if ( bb %in% authors ) {freq.table["other",bb] <- freq.table["other",bb] + 1}
          }
        }
        
      }
      
    }
  }
  #Delete those co-authors that are never show up as the first or last authors
  trial.table <- trial.table[relevant_authors,relevant_authors]
  
  return( list(data_table=trial.table,num_records=pubmed_search$count, freq_table=freq.table) )

}


