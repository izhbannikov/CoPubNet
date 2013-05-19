library(igraph)

balm.vizualize <- function(lines, gsize) {

  #lines=scan('test.csv',what="character",sep="\n",skip=1) # read the csv file (skipping the header), line-by-line as character string.
  lines=gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
  adjlist=strsplit(lines,",") # splits the character strings into list with different vector for each line
  col1=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
  col2=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
  el=cbind(col1,col2) # creates the edgelist by combining column 1 and 2.

  bsk.network<-graph.edgelist( el )

  # We can also color the connecting edges differently depending on the 'grade': 
  E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")

  # or depending on the different specialization ('spec'):

  E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

  V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
  V(bsk.network)$size <- ifelse(V(bsk.network)$size >= 20,  20, V(bsk.network)$size)
  
  graphs <- decompose.graph(bsk.network)
  #largest <- which.max(sapply(graphs, vcount))
  weak_connected <- which(sapply(graphs, vcount) <= strtoi(gsize, base = 0L) )
  
  if(length(weak_connected)) {
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
  plot(bsk.network,    		#the graph to be plotted
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

  
}

balm.pie <- function(lines, gsize, lbls, pchart_data) {
  par(mar=c(1, 1, 1, 1))
  par(oma=c(1, 1, 1, 1))
  head(pchart_data)
  
  lines=gsub("[ ]+$","",gsub("[ ]+"," ",lines)) # remove trailing and multiple spaces.
  adjlist=strsplit(lines,",") # splits the character strings into list with different vector for each line
  col1=unlist(lapply(adjlist,function(x) rep(x[1],length(x)-1))) # establish first column of edgelist by replicating the 1st element (=ID number) by the length of the line minus 1 (itself)
  col2=unlist(lapply(adjlist,"[",-1)) # the second line I actually don't fully understand this command, but it takes the rest of the ID numbers in the character string and transposes it to list vertically
  el=cbind(col1,col2) # creates the edgelist by combining column 1 and 2.
  
  bsk.network<-graph.edgelist( el )
  
  # We can also color the connecting edges differently depending on the 'grade': 
  E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")
  
  # or depending on the different specialization ('spec'):
  
  E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))
  
  V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
  
  graphs <- decompose.graph(bsk.network)
  
  strong_connected <- which(sapply(graphs, vcount) >= strtoi(gsize, base = 0L) )
  
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
    slices <- seq(1: dim(pchart_data)[1] )
    print(slices)
    for(l in seq(1:dim(pchart_data)[1])) {
      frame=pchart_data[l,V(graphs[[g]])$name]
      cnt = sum(frame[1,]) 
      slices[l] <- cnt
    }
    
    degr <- names(sort(degree(graphs[[g]])))
    #tslices <- slices[slices != 0]
    #tlbls <- lbls[ slices != 0 ]
    #print(tslices)
    #print(tlbls)
    #pie(tslices, labels = "", cex.main=1.3,cex=1.2,main=tail(degr,1),radius=0.7,col=rainbow(length(lbls)))  
    pie(slices, labels = "", cex.main=1.3,cex=1.2,main=tail(degr,1),radius=0.7,col=rainbow(length(lbls)))  
  }
}

balm.legend <- function(lbls) {
  # setup for no margins on the legend
  par(mar=c(0, 0, 0, 0))
  # c(bottom, left, top, right)
  plot.new()
  legend('center',lbls, lty=seq(1,length(lbls)),
         col=rainbow(length(lbls)),bty ="n")
  
}

balm.vizualize2 <- function(trial.table, gsize) {
  #print(trial.table)
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
  
  if(length(weak_connected)>3) {
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
  plot(bsk.network,      	#the graph to be plotted
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
  
  
}

balm.pie2 <- function(trial.table, gsize, lbls, pchart_data) {
  par(mar=c(1, 1, 1, 1))
  par(oma=c(1, 1, 1, 1))
  head(pchart_data)
  
  network_data <- trial.table[,2:ncol(trial.table)]
  bsk.network<-graph.adjacency( ceiling(trial.table) )
  
  # We can also color the connecting edges differently depending on the 'grade': 
  E(bsk.network)$color<-ifelse(E(bsk.network)$grade<=50, "red", "grey")
  
  # or depending on the different specialization ('spec'):
  
  E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))
  
  V(bsk.network)$size<-degree(bsk.network)#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
  
  graphs <- decompose.graph(bsk.network)
  
  strong_connected <- which(sapply(graphs, vcount) >= strtoi(gsize, base = 0L) )
  
  strong_connected
  
  #Layout
  matrix_layout =  matrix(c(1,2,3,4,5),1, 5,byrow=TRUE)
  
  max <- 5
  for(i in seq(1:ceiling(length(strong_connected)/5)) ) {
    seq_i <- seq(max+1,max+5)
    #print(seq_i) 
    matrix_layout <- rbind(matrix_layout, seq_i)
    max <- max+5
    #print(max)
  }
  
  df<-layout(matrix_layout, respect=TRUE)
  layout.show(df)
  
  for(g in strong_connected ) {
    cnt <- 0.0
    slices <- seq(1: dim(pchart_data)[1] )
    print(slices)
    for(l in seq(1:dim(pchart_data)[1])) {
      frame=pchart_data[l,V(graphs[[g]])$name]
      cnt = sum(frame) 
      slices[l] <- cnt
    }
    
    degr <- names(sort(degree(graphs[[g]])))
    pie(slices, labels = "", cex.main=1.3,cex=1.2,main=tail(degr,1),radius=0.7,col=rainbow(length(lbls)))  
  }
}
