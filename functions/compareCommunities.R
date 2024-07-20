compareCommunities <- function(
    estimated.factors = estimated.factors,
    estimated.factors2 = estimated.factors2
    ){

  if(is.object(estimated.factors)){
    stop("Error: estimated.factors must be an integer")
  }
  
  if(is.object(estimated.factors2)){
    stop("Error: estimated.factors2 must be an integer")
  }

  indices <- match(names(estimated.factors), names(estimated.factors2)) %>% na.omit()
  
  if(length(estimated.factors)==3){
    indices <- names(estimated.factors[indices])
  } else if(length(estimated.factors2)==3){
    indices <- names(estimated.factors[indices])
  } else if(length(estimated.factors2)==2){
    indices <- names(estimated.factors[indices])
  }else if(length(estimated.factors2)==2){
    indices <- names(estimated.factors[indices])
  }else if(length(estimated.factors2)==1){
    indices <- names(estimated.factors[indices])
  }else if(length(estimated.factors2)==1){
    indices <- names(estimated.factors[indices])
  }else {
    stop("Error: the community object is not correct")
  }
  
  
  PC <- list()
  for(i in indices){
    PC[[i]] <- sum(as.integer(estimated.factors[[i]]$membership == estimated.factors2[[i]]$membership))/10
  }
  names(PC) <- paste0("PC.",indices)
  
  library("Metrics")
  
  MAE <- list()
  for(z in indices){
    MAE[[z]] <- mae(estimated.factors[[z]]$membership, estimated.factors2[[z]]$membership)
  }
  names(MAE) <- paste0("MAE.",indices)

  MBE <- list()
  for(y in indices){
    MBE[[y]] <- Metrics::bias(estimated.factors[[y]]$membership, estimated.factors2[[y]]$membership)
  }
  names(MBE) <- paste0("MBE.",indices)

  library("igraph")
  
  ARI1 <- list()
  ARI2 <- list()
  for(g in indices){
    ARI1[[g]] <- igraph::compare(estimated.factors[[g]], estimated.factors2[[g]], method = "rand")
    ARI2[[g]] <- igraph::compare(estimated.factors[[g]], estimated.factors2[[g]], method = "adjusted.rand")
  }
  names(ARI1) <- paste0("ARI1.",indices)
  names(ARI2) <- paste0("ARI2.",indices)
  
  #Results[["PC.leiden"]] <- PC[["PC.leiden"]]
  #Results[["PC.walktrap"]] <- PC[["PC.walktrap"]]
  #Results[["PC.spinglass"]] <- PC[["PC.spinglass"]]
  
  #Results[["MAE.leiden"]] <- MAE[["MAE.leiden"]]
  #Results[["MAE.walktrap"]] <- MAE[["MAE.walktrap"]]
  #Results[["MAE.spinglass"]] <- MAE[["MAE.spinglass"]]
  
  #Results[["MBE.leiden"]] <- MBE[["MBE.leiden"]]
  #Results[["MBE.walktrap"]] <- MBE[["MBE.walktrap"]]
  #Results[["MBE.spinglass"]] <- MBE[["MBE.spinglass"]]
  
  #Results[["ARI1.leiden"]] <- ARI1[["ARI1.leiden"]]
  #Results[["ARI1.walktrap"]] <- ARI1[["ARI1.walktrap"]]
  #Results[["ARI1.spinglass"]] <- ARI1[["ARI1.spinglass"]]
  
  #Results[["ARI2.leiden"]] <- ARI2[["ARI2.leiden"]]
  #Results[["ARI2.walktrap"]] <- ARI2[["ARI2.walktrap"]]
  #Results[["ARI2.spinglass"]] <- ARI2[["ARI2.spinglass"]]
  
  Results <- list()
  Results$PC.leiden <- PC[["PC.leiden"]]
  Results$PC.walktrap <- PC[["PC.walktrap"]]
  Results$PC.spinglass <- PC[["PC.spinglass"]]
  
  Results$MAE.leiden <- MAE[["MAE.leiden"]]
  Results$MAE.walktrap <- MAE[["MAE.walktrap"]]
  Results$MAE.spinglass <- MAE[["MAE.spinglass"]]
  
  Results$MBE.leiden <- MBE[["MBE.leiden"]]
  Results$MBE.walktrap <- MBE[["MBE.walktrap"]]
  Results$MBE.spinglass <- MBE[["MBE.spinglass"]]
  
  Results$ARI1.leiden <- ARI1[["ARI1.leiden"]]
  Results$ARI1.walktrap <- ARI1[["ARI1.walktrap"]]
  Results$ARI1.spinglass <- ARI1[["ARI1.spinglass"]]
  
  Results$ARI2.leiden <- ARI2[["ARI2.leiden"]]
  Results$ARI2.walktrap <- ARI2[["ARI2.walktrap"]]
  Results$ARI2.spinglass <- ARI2[["ARI2.spinglass"]]
  
  Results$n.leiden <- length(unique(estimated.factors2$leiden$membership))
  Results$n.walktrap <- length(unique(estimated.factors2$walktrap$membership))
  Results$n.spinglass <- length(unique(estimated.factors2$spinglass$membership))
  
  Results

}

