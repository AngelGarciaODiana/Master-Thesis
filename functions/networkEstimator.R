# Common estimation function for all networks:
networkEstimator <- function(
  data,
  transformation = c("none", "rank", "quantile","npn","polychoric/categorical"),
  method = c("EBICglasso", "huge", "TMFG", "LoGo"),
  variant,
  ordinal
){
  library("qgraph")
  library("bootnet")
  library("mgm")
  library("BGGM")
  library("GGMnonreg")
  library("huge")
  library("psychonetrics")
  library("dplyr")
  # library("parcor")
  library("BDgraph")
  sampleAdjust  <-  "pairwise_average"
  
  if (missing(ordinal)){
    ordinal <- all(na.omit(unique(unlist(data))) %%1 == 0)
  }

  
  if (missing(variant)){
    if (method %in% c("EBICglasso","huge","TMFG","LoGo")){
      variant <- 1
    } else {
      variant <- 1
    }
  }
  
  transformation <- match.arg(transformation)
  method <- match.arg(method)
  
  # Some should give an error:
  if (!ordinal & transformation == "polychoric/categorical"){
    stop("Cannot treat continuous data as (ordered) categorical")
  }
 
  
  # Variant:
  alpha <- switch(variant, 
                  `1` = 0.005)
  
  # BF_cut <- switch(variant, 
  #                 `1` = 30,
  #                 `2` = 10,
  #                 `3` = 3)
  
  EBICtuning <- switch(variant, 
                       `1` = 0.5)
  hugetype <- switch(variant,
                     `1` = "mb")
  CVfolds <- switch(variant,
                     `1` = 20)
  BDalgorithm <- switch(variant,
                    `1` = "bdmcmc")
  

  # Transform data:
  if (transformation == "rank"){
    data <- as.data.frame(scale(bootnet::rank_transformation(data)))
  }
  if (transformation == "quantile"){
    data <- bootnet::quantile_transformation(data)
  }
  if (transformation == "npn"){
    if (any(is.na(data))){
      stop("'npn' transformation not supported for missing data")
    }
    data <- huge.npn(data)
  }
  
  # Check for psychonetrics:
  psychonetricscheck <- function(x){
    if (!psychonetrics:::sympd_cpp(x@information)) stop("Information matrix is not positive semi-definite.")
    return(x)
  }
  
  
  # Estimate model:
  if (method == "EBICglasso"){
    res <- estimateNetwork(data, default = "EBICglasso",
                           sampleSize = sampleAdjust,
                           tuning = EBICtuning,
                           corMethod = ifelse(transformation == "polychoric/categorical",
                                              "cor_auto",
                                              "cor"))
    
    estnet <- res$graph
  } else  if (method == "huge"){
    res <- estimateNetwork(data, default = "huge",
                           sampleSize = sampleAdjust,
                           tuning = EBICtuning,
                           corMethod = ifelse(transformation == "polychoric/categorical",
                                              "npn",
                                              "cor_auto"),
                           criterion = "ric")
    
    estnet <- res$graph
  } else if (method == "TMFG"){
    res <- estimateNetwork(data, default = "TMFG",
                           sampleSize = sampleAdjust,
                           tuning = EBICtuning,
                           graphType = ifelse(transformation == "polychoric/categorical",
                                              "npn",
                                              "cor"))
    
    estnet <- res$graph
  } else if (method == "LoGo"){
    res <- estimateNetwork(data, default = "LoGo",
                           sampleSize = sampleAdjust,
                           tuning = EBICtuning,
                           corMethod = ifelse(transformation == "polychoric/categorical",
                                              "npn",
                                              "cor_auto"))
    
    estnet <- res$graph
  } 

  return(as.matrix(estnet))
}
  
