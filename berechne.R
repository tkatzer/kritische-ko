

berechne<- function(coocTerm, numberOfCoocs, binDTM){
  
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
  # Display the numberOfCoocs main terms
  print(coocs[1:numberOfCoocs])
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  # Sample of some examples from resultGraph
  resultGraph[sample(nrow(resultGraph), 6), ]
  
  require(igraph)
  
  # Set the graph and type
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  
  # Identification of all nodes with less than 2 edges
  graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  graphNetwork <- delete.vertices(graphNetwork, graphVs) 
  
  # Assign colors to edges and nodes (searchterm blue, rest orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
  
  # Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
  halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
  E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
  
  # Disable edges with radius
  E(graphNetwork)$curved <- 0 
  # Size the nodes by their degree of networking
  V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
  
  # All nodes must be assigned a standard minimum-size
  V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
  
  # edge thickness
  E(graphNetwork)$width <- 2
  
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  
  
  library(visNetwork)
  library(geomnet)
  data <- toVisNetworkData(graphNetwork)
  visNetwork(nodes = data$nodes, edges = data$edges, physics = TRUE, height = "1000px", width = "1000px")%>%
    visPhysics(stabilization = TRUE, barnesHut = list(avoidOverlap = 1))%>%
    visEdges(smooth = FALSE)
  
  
}

