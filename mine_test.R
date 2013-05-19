library(rentrez)
library(igraph)

#=======================================================================================================
search_limit <- 1000
pubmed_search <- entrez_search(db = "pubmed", term = "ph[and]vagina[and]bacteria",retmax=search_limit,mindate="2010", maxdate="2013")
#pubmed_search <- entrez_search(db = "pubmed", term = "white owen",retmax = 1000,mindate="2010", maxdate="2013")

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

#ifelse(search_limit < pubmed_search$count, search_limit, pubmed_search$count)

while(ii< ifelse(search_limit < pubmed_search$count, search_limit, pubmed_search$count) ) {
  print("Limits")
  print(search_limit)
  #print(pubmed_search$ids[ii:(ii+100)])
  summaries <- entrez_summary(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],mindate="2010", maxdate="2013")
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
  
  abstracts1 <- entrez_fetch(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],file_format='abstract',retmode='text',mindate="2010", maxdate="2013")
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
#print(freq.table)
gsize <- 5

network_data <- trial.table[,2:ncol(trial.table)]
bsk.network<-graph.adjacency( ceiling(trial.table) )

# We can also color the connecting edges differently depending on the 'grade': 
E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")

# or depending on the different specialization ('spec'):

E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
V(bsk.network)$size <- ifelse(V(bsk.network)$size >= 10,  10, V(bsk.network)$size)

graphs <- decompose.graph(bsk.network)
#largest <- which.max(sapply(graphs, vcount))
weak_connected <- which(sapply(graphs, vcount) < strtoi(gsize, base = 0L) )
if(length(weak_connected) > 3) {
  bad.vs <- V(graphs[[weak_connected[1]]])$name
  for(i in seq(2,length(weak_connected-1))) {
    bad.vs <- append(bad.vs, V(graphs[[weak_connected[i]]])$name )
  }

  bad.vs
  bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph
}

#layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10,11,12,13), 7, 4, byrow=TRUE), respect=TRUE)
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(0.5, 0.5, 0.5, 0.5))

#layout.show(nf)
#par(mai=c(0,0,1,0))       #this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,        #the graph to be plotted
     layout=layout.fruchterman.reingold(bsk.network,  area=vcount(bsk.network)^2.1, repulserad=vcount(bsk.network)^2.3),	# the layout method. see the igraph documentation for details
     #main='Co-occurrence with any of the word in Group C with any of the word in Group B',	#specifies the title
     edge.arrow.width = 0,
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=0.1,			#the font of the name labels
     vertex.label= V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
)

####Integration with RCPP
library(mine)

#Here we include our .cpp file with useful functions

kwords <- c("L. iners", "L. crispatus", "L. gasseri", "L. jensenii")

search_limit <- 1000
pubmed_search <- entrez_search(db = "pubmed", term = "ph[and]vagina[and]bacteria",retmax=search_limit,mindate="2010", maxdate="2013")
ii <- 1
authors <- c()
authors2 <- c()
titles <- c()
abstracts <- c()

while(ii< ifelse(search_limit < pubmed_search$count, search_limit, pubmed_search$count) ) {
  print("Limits")
  print(search_limit)
  summaries <- entrez_summary(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],mindate="2010", maxdate="2013")
  authors <- c(authors,unique(xpathSApply(summaries, "//Item[@Name='Author']",xmlValue)))
  titles <- c(titles,xpathSApply(summaries, "//Item[@Name='Title']", xmlValue))
  authors2 <- c(authors2,xpathSApply(summaries, "//Item[@Name='AuthorList']"))
  
  abstracts1 <- entrez_fetch(db = "pubmed", ids = pubmed_search$ids[ii:(ii+100)],file_format='abstract',retmode='text',mindate="2010", maxdate="2013")
  abstracts <- c(abstracts,strsplit(abstracts1, "\n\n\n")[[1]])
  ii <- ii+100
  print(ii)
}


report.table <- matrix("", nrow=length(abstracts)*length(kwords),ncol=4,byrow=TRUE)
colnames(report.table) <- c("organism","n","PMID","authors")
#report.table <- as.table(report.table)

for(i in seq(length(kwords),length(abstracts)*length(kwords),by=length(kwords))) {
  ans<-process_text(abstracts[i/length(kwords)],kwords)
  
  for(j in seq(0,length(kwords)-1)) {
    report.table[i+j-length(kwords),1] <- kwords[j+1]
    report.table[i+j-length(kwords),2] <- toString(ans[j+1])
    report.table[i+j-length(kwords),3] <- pubmed_search$ids[i/length(kwords)]
    #report.table[i+j-length(kwords),4] <- ""
    
  }
}


