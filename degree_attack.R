degree_attack <- function(g, inverse = FALSE){
  require(sna)
  d = degree(g, gmode = "graph")
  n = length(d)
  if(inverse == FALSE) weights = d/sum(d)
  else weights = sum(d)/(d + 0.00000001)
  nodes = sample(n, n/2, prob = weights)
  for (node in nodes){
    inds = which(g[,1] == node)
    if(length(inds) == 0) next #in case the node has no edges
    ind = sample(inds, 1)
    adj = g[ind, 2]
    ind.2 = which(g[,1] == adj & g[,2] == node) #find the symmetric row
    g[c(ind, ind.2),3] = 0  
  }
  return(g)
}