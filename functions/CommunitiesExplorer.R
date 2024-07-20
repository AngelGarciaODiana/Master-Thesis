communitiesExplorer <- function(
  truenetwork,
  objective_function = c("CPM", "modularity"),
  n_iterations = 5000,
  resolution_parameter = 0.8,
  steps = 5,
  spins = 5000,
  gamma = 0.8,
  update_rule = c("simple","config"),
  implementation = c("orig","neg"),
  seed = 123
){
  library("EGAnet")
  data <- convert2igraph(truenetwork, diagonal = 0)
  
  Results <- list()
  
  library("igraph")

  
  if(any(truenetwork<0)==TRUE){
    warning("The network is not positive at all so, only leiden is calculed")
    
    leiden <- cluster_leiden(data, 
                             objective_function = objective_function, 
                             n_iterations = n_iterations, 
                             resolution_parameter = resolution_parameter)
    
    Results$leiden <- leiden
    
  } else {
  leiden <- cluster_leiden(data, 
                           objective_function = objective_function, 
                           n_iterations = n_iterations, 
                           resolution_parameter = resolution_parameter)
  
  Results$leiden <- leiden
  
  walktrap <- cluster_walktrap(data, 
                               steps = steps, 
                               modularity = TRUE, 
                               membership = TRUE) 
  
  Results$walktrap <- walktrap
  
  set.seed(seed)
  spinglass <- cluster_spinglass(data, 
                                 spins = spins,
                                 gamma = gamma, 
                                 update.rule = update_rule,
                                 implementation = implementation)
  
  Results$spinglass <- spinglass  
  }
  

  
  Results
  
}
