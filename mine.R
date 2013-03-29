library(rentrez)

mine <- function(search_string,search_limit,kterms,from_date,to_date) {
  #pubmed_search <- entrez_search(db = "pubmed", term = "ph[and]vagina[and]bacteria",retmax=5)
  pubmed_search <- entrez_search(db = "pubmed", term = search_string,retmax = search_limit,mindate=from_date, maxdate=to_date)
  
  #summaries <- entrez_summary(db = "pubmed", ids = pubmed_search$ids,mindate=from_date, maxdate=to_date)
  #authors <- unique(xpathSApply(summaries, "//Item[@Name='Author']",xmlValue))
  #titles <- xpathSApply(summaries, "//Item[@Name='Title']", xmlValue)
  #print(summaries)
  
  #abstracts <- entrez_fetch(db = "pubmed", ids = pubmed_search$ids,file_format='abstract',retmode='text',mindate=from_date, maxdate=to_date)
  #abstracts <- c(strsplit(abstracts, "\n\n\n"))
  
  ii <- 1
  authors <- c()
  authors2 <- c()
  titles <- c()
  abstracts <- c()
  
  while(ii<search_limit) {
    print("Limits")
    print(search_limit)
    #print(pubmed_search$ids[ii:(ii+100)])
    summaries <- entrez_summary(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],mindate=from_date, maxdate=to_date)
    authors <- c(authors,unique(xpathSApply(summaries, "//Item[@Name='Author']",xmlValue)))
    titles <- c(titles,xpathSApply(summaries, "//Item[@Name='Title']", xmlValue))
    #print(summaries)
    authors2 <- c(authors2,xpathSApply(summaries, "//Item[@Name='AuthorList']"))
    #First author
    #relevant_authors<-xpathSApply(summaries, "//Item[@Name='AuthorList']",function(x) xmlValue(xmlChildren(x)$Item))
    #Last author
    #relevant_authors<-c(relevant_authors,xpathSApply(summaries, "//Item[@Name='LastAuthor']",xmlValue))
    #relevant_authors <- unique(relevant_authors)
    #relevant_authors <- relevant_authors[is.element(relevant_authors,authors)]
    
    abstracts1 <- entrez_fetch(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],file_format='abstract',retmode='text',mindate=from_date, maxdate=to_date)
    abstracts <- c(abstracts,strsplit(abstracts1, "\n\n\n")[[1]])
    ii <- ii+100
    print(ii)
  }
  
  ii <- 1
  
  
  key_terms <- c(strsplit("bacterial vaginosis, yeast infection, preterm birth",','))
  
  trial <- matrix(0,ncol=length(authors),nrow=length(authors))
  colnames(trial) <- c(authors)
  rownames(trial) <- c(authors)
  trial.table <- as.table(trial)
  
  #Pie chart
  freq <- matrix(0,ncol=length(authors),nrow=length(kterms)+1)
  colnames(freq) <- c(authors)
  rownames(freq) <- c(names(kterms),"other")
  freq.table <- as.table(freq)
  #print(authors)
  relevant_authors <- c()
  #authors2 <- c(xpathSApply(summaries, "//Item[@Name='AuthorList']"))
  #print(authors2)
  for(k in seq(1, length(authors2)) ) {
    if(length(xmlToList(authors2[[k]]))-1 > 1) {
      b <- sapply(1:(length(xmlToList(authors2[[k]]))-1), function(i) {xmlToList(authors2[[k]])[i]$Item$text} )
      
      if( is.element(b[1],authors) & is.element(tail(b,1),authors) ) {relevant_authors <- c(relevant_authors, b[1],tail(b,1))}
      
      comb <- combn(b, 2)
      for(i in seq(1:dim(comb)[2])) {
        if ( (comb[,i][1] %in% authors) & (comb[,i][2] %in% authors) ) {
          trial.table[ comb[,i][1], comb[,i][2] ] <- trial.table[ comb[,i][1], comb[,i][2] ] + 1
        } 
      }
      
      for( term in names(kterms) ) {
        
        cnt <- 0
        if( length(grep(term, titles[k], ignore.case = T)) > 0 ) {
          cnt <- 1
          if ( is.element(b[1], authors )) {freq.table[term,b[1]] <- freq.table[term,b[1]] + 1}
          if ( is.element(tail(b,1), authors )) {freq.table[term,tail(b,1)] <- freq.table[term,tail(b,1)] + 1}
        }
        #  #Look for term in an abstract
        if( length(grep(term, abstracts[[1]][k], ignore.case = T)) > 0 ) {
          cnt <- 1
          if ( is.element(b[1], authors )) {freq.table[term,b[1]] <- freq.table[term,b[1]] + 0.5}
          if ( is.element(tail(b,1), authors )) {freq.table[term,tail(b,1)] <- freq.table[term,tail(b,1)] + 0.5}
        }
        
        #  #Look for the other related terms in an abstract and a title:
        for(relterm in kterms[[term]]) {
          if( length(grep(relterm, titles[k], ignore.case = T)) > 0 ) {
            cnt <- 1
            if ( is.element(b[1], authors )) {freq.table[term,b[1]] <- freq.table[term,b[1]] + 1}
            if ( is.element(tail(b,1), authors )) {freq.table[term,tail(b,1)] <- freq.table[term,tail(b,1)] + 1}
          }
          if( length(grep(relterm, abstracts[[1]][k], ignore.case = T)) > 0 ) {
            cnt <- 1
            if ( is.element(b[1], authors )) {freq.table[term,b[1]] <- freq.table[term,b[1]] + 0.5}
            if ( is.element(tail(b,1), authors )) {freq.table[term,tail(b,1)] <- freq.table[term,tail(b,1)] + 0.5}
          }
          
        }
        
        if(cnt == 0) {
          if ( is.element(b[1], authors )) {freq.table["other",b[1]] <- freq.table["other",b[1]] + 1}
          if ( is.element(tail(b,1), authors )) {freq.table["other",tail(b,1)] <- freq.table["other",tail(b,1)] + 1}
        }
        
      }
      
    }
  }
  #Delete those co-authors that are never show up as the first or last authors
  relevant_authors <- unique(relevant_authors)
  trial.table <- trial.table[relevant_authors,relevant_authors]
  
  return( list(data_table=trial.table,num_records=pubmed_search$count, freq_table=freq.table) )

}


