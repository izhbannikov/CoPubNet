library(rentrez)
library(igraph)

  kterms <- c("bacterial vaginosis", "yeast infection", "preterm birth")
  pubmed_search <- entrez_search(db = "pubmed", term = "ph[and]vagina[and]bacteria",retmax=5)
  pubmed_search$count

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
#print(freq.table)  
#Delete those co-authors that are never show up as the first or last authors
trial.table <- trial.table[relevant_authors,relevant_authors]



network_data <- trial.table[,2:ncol(trial.table)]
bsk.network<-graph.adjacency( ceiling(trial.table) )  
E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")

# or depending on the different specialization ('spec'):

E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
V(bsk.network)$size <- ifelse(V(bsk.network)$size >= 20,  20, V(bsk.network)$size)

graphs <- decompose.graph(bsk.network)
weak_connected <- which(sapply(graphs, vcount) <= strtoi(3, base = 0L) )
bad.vs <- V(graphs[[weak_connected[1]]])$name

for(i in seq(2,length(weak_connected-1))) {
  bad.vs <- append(bad.vs, V(graphs[[weak_connected[i]]])$name )
}

#bad.vs
bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph


#layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10,11,12,13), 7, 4, byrow=TRUE), respect=TRUE)
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(0.5, 0.5, 0.5, 0.5))

#layout.show(nf)
#par(mai=c(0,0,1,0))       #this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,        #the graph to be plotted
     layout=layout.fruchterman.reingold(bsk.network,  area=vcount(bsk.network)^2.1, repulserad=vcount(bsk.network)^2.5),	# the layout method. see the igraph documentation for details
     #main='Co-occurrence with any of the word in Group C with any of the word in Group B',	#specifies the title
     edge.arrow.width = 0,
     vertex.label.dist=0.1,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=0.1,			#the font of the name labels
     vertex.label= V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1.3			#specifies the size of the font of the labels. can also be made to vary
)


par(mar=c(1, 1, 1, 1))
par(oma=c(1, 1, 1, 1))
head(freq.table)

network_data <- trial.table[,2:ncol(trial.table)]
bsk.network<-graph.adjacency( ceiling(trial.table) )

# We can also color the connecting edges differently depending on the 'grade': 
E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")

# or depending on the different specialization ('spec'):

E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)

graphs <- decompose.graph(bsk.network)

strong_connected <- which(sapply(graphs, vcount) >= strtoi(3, base = 0L) )

strong_connected

#Layout
matrix_layout =  matrix(c(1,2,3,4),1, 4,byrow=TRUE)

max <- 4
for(i in seq(1:ceiling(length(strong_connected)/4)) ) {
  seq_i <- seq(max+1,max+4)
  #print(seq_i) 
  matrix_layout <- rbind(matrix_layout, seq_i)
  max <- max+4
  #print(max)
}

df<-layout(matrix_layout, respect=TRUE)
layout.show(df)

for(g in strong_connected ) {
  cnt <- 0.0
  slices <- seq(1: dim(freq.table)[1] )
  print(slices)
  for(l in seq(1:dim(freq.table)[1])) {
    frame=freq.table[l,V(graphs[[g]])$name]
    print(frame)
    cnt = sum(frame) 
    slices[l] <- cnt
  }
  
  degr <- names(sort(degree(graphs[[g]])))
  pie(slices, labels = "", cex.main=1.3,cex=1.2,main=tail(degr,1),radius=0.7,col=rainbow(length(kterms)))  
}
