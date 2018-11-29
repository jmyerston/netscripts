# Created on August 15 2018
# @authors: Colin Burke and Jacobo Myerston

# load package's
library(igraph)
library(stringi)
library(tidyverse)

# function to rescale node degree
rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

## IMPORTANT: All files must be in your working directory (wd) ##

# Check working directory if needed
getwd()

# Set working directory if needed
setwd('/Users/jacobo/OneDrive/OneDrive\ -\ UC\ San\ Diego/Archaic\ Social\ Networks/R\ Scripts/')

# Import node list: contains nodes$Name, nodes$Group
nodes <- read.csv('new_Nodes.csv', 
                  sep=",", 
                  header = TRUE, 
                  col.names = c("Name","Group"),
                  encoding = "UTF-8")

# Import edge list: contains edges$Source, edges$Target, edges$Weight, edges$Relation
edges <- read.csv('new_Edges.csv',
                  header = TRUE, 
                  sep=",",
                  col.names = c("Source","Target","Relation"),
                  encoding = "UTF-8")

# Remove leading and trailing whitespace
nodes$Name <- stri_trim(nodes$Name)
nodes$Group <- stri_trim(nodes$Group)
edges$Source <- stri_trim(edges$Source)
edges$Target <- stri_trim(edges$Target)
edges$Relation <- stri_trim(edges$Relation)

# Create undirected graph
# Utilizando v?rtices como par?metros se crea la red con los atributos adicionales en un solo paso
g <- graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)

# Verifica consistencia vertices - ejes
edges_ = c(edges$Source,edges$Target)
length(edges_)
missing = edges_ %in% nodes$Name
missing_indx = which(missing == FALSE)
edges_[missing_indx]
table(edges_ %in% nodes$Name)

## 'Male', 'Female' y 'Geography' deben tener colores distintos
V(g)$color <- case_when(
  V(g)$Group == "Male" ~ 'red',
  V(g)$Group == "Female" ~ 'orange',
  V(g)$Group ==  "Place" ~ 'blue'
)

# Es necesario definir una columna de id para exportar correctamente al formato Pajek
V(g)$id = V(g)$name

# Scale node label size (min deg., max deg., min size, max size)
labsize <- rescale(degree(g), min(degree(g)), max(degree(g)), 0.1, 1)
V(g)$label.cex <- labsize

# Scale node size (min deg., max deg., min size, max size)
deg <- rescale(degree(g), min(degree(g)), max(degree(g)), 3, 15)
V(g)$size <- deg

# Plot unweighted network graph 
plot(g,
     layout=layout.fruchterman.reingold(g, niter=10000),
     edge.width=1, 
     edge.arrow.size = 20, 
     edge.curved=FALSE,
     vertex.label.color= 'black', 
     main = "Archaic Social Networks", 
     sub = "Full Graph")

# Plot  relations "is friend of" "is teacher of" "is associated with" 
g.new <- subgraph.edges(g,
                        which(
                          E(g)$Relation == "is friend of" |
                          E(g)$Relation == "is teacher of" |
                          E(g)$Relation=="is family of" |
                          E(g)$Relation== "studied the work of"))

plot(g.new,
     main = "Archaic Social Networks",
     sub = "Edges: is friend of, is teacher of, is associated with, traveled to")

# Plot  relation "is friend of
gf <- subgraph.edges(g,
                     which(E(g)$Relation =="is friend of"))
plot(gf, 
     main = "Archaic Social Networks", 
     sub = "Edges: is friend of")

# Plot  relation "is teacher of"
gt <- subgraph.edges(g,
                     which(E(g)$Relation == "is teacher of"))
plot(gt, 
     main = "Archaic Social Networks", 
     sub = "Edges: is teacher of")

# Plot  relation "is teacher of"
gtt <- subgraph.edges(g,
                      which(E(g)$Relation == "is teacher of" | E(g)$Relation== "traveled to"))
plot(gtt, 
     main = "Archaic Social Networks", 
     sub = "Edges: is teacher of, traveled to")

#Democritus Egonet
d <- make_ego_graph(g.new,
                    7, 
                    "Democritus", 
                    mode = c("all"), 
                    mindist = 1)
plot(d[[1]], 
     main = "Archaic Social Networks", 
     sub = "Democritus Egonet")

# Export to pajek format
write_graph(g, "pajek_g.txt", "pajek")
write_graph(gf, "pajek_gf.txt", "pajek")
write_graph(gt, "pajek_gt.txt", "pajek")
write_graph(gtt, "pajek_gtt.txt", "pajek")
write_graph(g.new, "pajek_gnew.txt", "pajek")

