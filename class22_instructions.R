# Part 1: phylogenetic trees

# Load packages (remember to install.packages() as necessary)
library("ape")
library("phytools")
library("tidyverse")


# Make a phylogeny by hand
text.string <- "(((((((cow, pig),whale),(bat,(lemur,human))),(robin,iguana)),coelacanth),gold_fish),shark);"
vert.tree <- read.tree(text=text.string)

# Plot the tree a few ways
plot(vert.tree,no.margin=TRUE,edge.width=2)

roundPhylogram(vert.tree)

plot(unroot(vert.tree),type="unrooted",no.margin=TRUE,lab4ut="axial",
     edge.width=2)


# Look at the tip labels
vert.tree$tip.label



# Load a different tree from a file

anolis.tree <- read.tree(file="anolis.tre")
anolis.tree

# Make some plot of anolis.tree as above

# Now, make a plot using a different package - can you make a circular tree using the guide found here?
# https://guangchuangyu.github.io/ggtree-book/chapter-ggtree.html



# Make a different tree and add in internal node images
tree <- read.nexus("https://raw.githubusercontent.com/rgriff23/Dissertation/master/Chapter_2/data/tree.nex")
phylopic_info <- data.frame(node = c(124, 113, 110, 96, 89, 70),
                            phylopic = c("7fb9bea8-e758-4986-afb2-95a2c3bf983d",
                                         "bac25f49-97a4-4aec-beb6-f542158ebd23",
                                         "f598fb39-facf-43ea-a576-1861304b2fe4",
                                         "aceb287d-84cf-46f1-868c-4797c4ac54a8",
                                         "0174801d-15a6-4668-bfe0-4c421fbe51e8",
                                         "72f2f854-f3cd-4666-887c-35d5c256ab0f"),
                            species = c("galagoids", "lemurs", "tarsiers",
                                        "cebids", "hominoids", "cercopithecoids"))
pg <- ggtree(tree)                                      
pg %<+% phylopic_info + geom_nodelab(aes(image=phylopic), geom="phylopic", alpha=.5, color='steelblue')


# How would you add tip labels to this figure?





# Part 2: Networks
library('igraph')

# Reproduce the figures shown in the lecture
sw <- sample_smallworld(dim=2, size=10, nei=1, p=0.1)
plot(sw, vertex.size=6, vertex.label=NA)

ba <-  sample_pa(n=100, power=1, m=1,  directed=F)
plot(ba, vertex.size=6, vertex.label=NA)


# Now plot each in different layouts. In the plot function, add "layout = ..."
# Check out these for help:
?plot.igraph
?layout_


# Now do a ggplot version of a food web figure
library("bipartite")
library("intergraph")
library("ggnetwork")
library("network")

# Read in a dataframe
food_web <- read.csv("food_web.csv", header = T)[,-1]
food_web_mat <- graph_from_data_frame(food_web) %>% as_adjacency_matrix() %>% as.matrix()
food_web_net <- as.network(food_web_mat, directed = F, bipartite = F)

is_predator <- ifelse(network::get.vertex.attribute(food_web_net, attrname = "vertex.names") %in% food_web[,1],
                      "yes", "no")
food_web_net <- network::set.vertex.attribute(food_web_net, # the name of the network object
                                   "Mammal_predator", # the name we want to reference the variable by in that object
                                   is_predator # the value we are giving that variable
)

ggplot(food_web_net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black") +
  geom_nodelabel_repel(aes(label = vertex.names),
                       fontface = "bold", box.padding = unit(1, "lines")) +
  geom_nodes(aes(colour = Mammal_predator), size = 8) +
  theme_blank()

# What would you change to make this look nicer?



# Check out the ggnetwork package vignette and see if you can reproduce a figure shown there:
# https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html

