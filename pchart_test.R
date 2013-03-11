par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(0.5, 0.5, 0.5, 0.5))
pchart_data = read.csv(file="freqs.csv",head=TRUE,sep=",")
head(pchart_data)
lines <- scan('test.csv',what="character",sep="\n",skip=1)
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
operator <- ">"
gsize <- "6"
if (operator == "=") { strong_connected <- which(sapply(graphs, vcount) == strtoi(gsize, base = 0L) ) }
if (operator == ">") { strong_connected <- which(sapply(graphs, vcount) >= strtoi(gsize, base = 0L) ) }
if (operator == "<") { strong_connected <- which(sapply(graphs, vcount) <= strtoi(gsize, base = 0L) ) }

strong_connected

#Layout
matrix_layout =  matrix(c(1,2,3,4,5),1, 5,byrow=TRUE)
max <- 5
for(i in seq(1:ceiling(length(strong_connected)/5)) ) {
  seq_i <- seq(max+1,max+5)
  print(seq_i) 
  matrix_layout <- rbind(matrix_layout, seq_i)
  max <- max+5
  print(max)
}

df<-layout(matrix_layout, respect=TRUE)
layout.show(df)

for(g in strong_connected ) {
  cnt <- 0.0
  slices <- seq(1: dim(pchart_data)[1] )
  
  for(l in seq(1:dim(pchart_data)[1])) {
    frame=pchart_data[l,V(graphs[[g]])$name]
    cnt = sum(frame[1,]) #bv
    slices[l] <- cnt
  }
  
  
  
  #print(paste(names(frame), collapse = '\n'))
  degr = names(sort(degree(graphs[[g]])))
  lbls <- c("bacterial vaginosis", "yeast infection", "preterm birth","other")
  tslices <- slices[slices != 0]
  tlbls <- lbls[ slices != 0 ]
  print(tslices)
  print(tlbls)
  pie(tslices, labels = tlbls, cex.main=1.3,cex=1.2,main=tail(degr,1),radius=0.5,col=rainbow(length(lbls)))
}
