rm(all,Comm.Comp,CommComp,Comms,fig1,legend,mae_continuous,mbe_continuous,msgEnv,
   Net.Comp,NetComp,Nets,pc_continuous,summary.method,summary.method.cont,COLORS,i)

rm(mast,MAST_net,MMSE_net,SSR_net,comm.mast,comm.mmse,comm.ssr,mmse)

rm(Comm.Comp,CommComp,Comms,msgEnv,Net.Comp,NetComp,Nets,i)

method[[1]] <- "LoGo"



qgraph::smallworldness(MAST_net)
qgraph::smallworldIndex(qgraph(MAST_net$graph, groups=factor(truecommunities$leiden$membership)))

qgraph::smallworldIndex(qgraph(MMSE_net$graph, groups=factor(truecommunities$walktrap$membership)))


qgraph::qgraph.animate(MAST_net$graph,, groups=factor(truecommunities$leiden$membership),
                       legend=F,color=COLORS,theme="TeamFortress",labels=1:11, 
                       layout = "spring", edge.labels = F, edge.label.bg = F, 
                       edge.label.color = "#000100", edge.label.cex = 1.5,
                       border.color = "#f0f3fa", node.width = 0.8, label.cex = 2,
                       label.color = "#000100")



BDgraph::sparsity(matrix(as_adjacency_matrix(net.js),11,11))
BDgraph::sparsity(matrix(as_adjacency_matrix(net.js2),27,27))
igraph::mean_distance(net.js)

sum(rowSums(MAST_net$graph),colSums(MAST_net$graph))/(2*11)
sum(rowSums(MMSE_net$graph),colSums(MMSE_net$graph))/(2*27)


asd <- MAST_net$graph
asd[asd== 0] <- NA
sdf <- MMSE_net$graph
sdf[sdf== 0] <- NA

min(asd,na.rm=T)
max(MAST_net$graph)

min(sdf,na.rm=T)
max(MMSE_net$graph)



library(threejs)
library(htmlwidgets)
library(igraph)
net.js <- EGAnet::convert2igraph(MAST_net$graph)
graph_attr(net.js, "layout") <- NULL 
net.js$color.border <- "black"
new_cols <- c(COLORS)[membership(truecommunities$leiden)]
V(net.js)$color <- new_cols
#E(net.js)$width <- E(net.js)$weight + min(E(net.js)$weight) + 1 #Not Yet Supported


gjs <- graphjs(net.js, main="MAST", bg="gray10", showLabels=F, stroke=F, 
               curvature=0.1, attraction=0.9, repulsion=0.8, opacity=0.9)
print(gjs)
saveWidget(gjs, file="MAST-network-3D.html")
browseURL("MAST-network-3D.html")




net.js2 <- EGAnet::convert2igraph(MMSE_net$graph)
graph_attr(net.js2, "layout") <- NULL 
net.js2$color.border <- "black"
new_cols <- c(paletteer::paletteer_d("ggsci::springfield_simpsons"))[membership(truecommunities$walktrap)]
V(net.js2)$color <- new_cols
#E(net.js)$width <- E(net.js)$weight + min(E(net.js)$weight) + 1 #Not Yet Supported


gjs2 <- graphjs(net.js2, main="MMSE", bg="gray10", showLabels=F, stroke=F, 
               curvature=0.1, attraction=0.9, repulsion=0.8, opacity=0.9, 
               vertex.shape="sphere", fpl=300, vertex.size = 3,
               layout = list(layout_on_sphere(net.js2), layout_with_fr(net.js2, dim=3)), 
               brush=T)
print(gjs2)
saveWidget(gjs2, file="MMSE-network-3D-mov.html")
browseURL("MMSE-network-3D.html")
