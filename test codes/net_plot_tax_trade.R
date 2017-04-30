Trade_dyads<-read.csv("~/Downloads/COW_Trade_3.0/dyadic_trade_3.0.csv",stringsAsFactors=F)
Trade_dyads<-Trade_dyads[Trade_dyads$year==2006,]
head(Trade_dyads_b)
Trade_dyads
Trade_dyads_a<-Trade_dyads[,c("importer1", "importer2", "flow1")]
Trade_dyads_b<-Trade_dyads[,c("importer2", "importer1", "flow2")]
names(Trade_dyads_b) <- c("importer1", "importer2", "flow1")
Trade_dyads_ab<-rbind(Trade_dyads_a, Trade_dyads_b)
head(Trade_dyads_ab)
Trade_dyads_ab$flow1 <- rank(Trade_dyads_ab$flow1)

###make /gdp figure

international_economic_data<-read.csv("~/GitHub/SSHRC_Replication_File/international_economy.csv", header = TRUE)
names(international_economic_data)[1]<-paste("importer1")
international_economic_data<-international_economic_data[international_economic_data$Time==2006,]
international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.<-as.numeric(gsub(",", "", international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.))

Trade_dyads_abc <- merge(international_economic_data, Trade_dyads_ab, by= c("importer1"))

Trade_dyads_abc$imp_gdp<-(Trade_dyads_abc$flow1*10000000)/Trade_dyads_abc$GDP..constant.2005.US....NY.GDP.MKTP.KD.
Trade_dyads_abcd<-Trade_dyads_abc[,c("importer1", "importer2", "imp_gdp")]
Trade_dyads_abcd$imp_gdp[Trade_dyads_abcd$imp_gdp<1]<-0
Trade_dyads_abcd$imp_gdp[Trade_dyads_abcd$imp_gdp>0]<-1
###now do drop NA
Trade_dyads_abcd<-na.omit(Trade_dyads_abcd)
print(Trade_dyads_abcd[Trade_dyads_abcd$imp_gdp==1,])

##
#try to do as rank variable
head(Trade_dyads_ab)
Trade_dyads_abcde<-aggregate(Trade_dyads_ab$flow1, by=list(Trade_dyads_ab$importer1), FUN=rank)
Trade_dyads_abcde
by(Trade_dyads_ab, Trade_dyads_ab$importer1, rank)

library(dplyr)
trades_for_all<- Trade_dyads_ab %>%
  group_by(importer1) %>%
  mutate(my_ranks = order(as.numeric(flow1), decreasing=TRUE))
trades_for_all
Trade_dyads_ab

new_trade_data<-transform(Trade_dyads_ab,rank=ave(1:nrow(Trade_dyads_ab),importer2,
                      FUN=function(x) order(as.numeric(flow1[x]),decreasing=TRUE)))
new_trade_data[new_trade_data$importer1=="United States of America",]
##another alternate... I want three biggest... but also to keep weights

Trade_dyads_ab$trade_rank<-with(Trade_dyads_ab, ave(as.numeric(flow1), importer1, FUN=function(x) rev(rank(x))))
head(Trade_dyads_ab)

###
###
###
library(data.table)
Trade_dyads_ab_trades <- as.data.table(Trade_dyads_ab)
Trade_dyads_ab_trades[,tr_rank:=rank(-flow1,ties.method="first"),by=importer1]
###
Trade_dyads_ab_trades[,tr_rank_three := tr_rank>0 & tr_rank<3]
Trade_dyads_ab_trades$tr_rank_three[Trade_dyads_ab_trades$tr_rank_three=="TRUE"]<-1
Trade_dyads_ab_trades$tr_rank_three[Trade_dyads_ab_trades$tr_rank_three=="FALSE"]<-0
Trade_dyads_ab_trades
###
###
Trade_dyads_ab_trades[Trade_dyads_ab_trades$importer1=="Canada",]

Trade_dyads_ab_trade<-transform(Trade_dyads_ab, x= ave(flow1,importer1,FUN=function(x) order(flow1,decreasing=T)))

head(Trade_dyads_ab_trade)

summary(Trade_dyads_abcd$imp_gdp)
print(Trade_dyads_abcd[Trade_dyads_abcd$imp_gdp==1,])
Bes_Per_Eng_Fr_sa<-merge(Bes_Per, state_antiquity, by = c("ncode"))
head(international_economic_data)
?merge
Trade_dyads_ab$flow1 <- rank(Trade_dyads_ab$flow1)

head(Trade_dyads_ab)
?rank


###
###
###
n_nodes<-nrow(Trade_dyads_ab)
edge_values<-Trade_dyads_ab$flow1
trade_matrix<-matrix(edge_values, nrow = n_nodes, ncol = n_nodes)
diag(trade_matrix) <- 0

summary(Trade_dyads_ab$flow1[Trade_dyads_ab$importer1=="United States of America"])


###
###
###
###
#Trade_dyads_ab
links <- read.csv("edges.csv")
nodes <- read.csv("limited nodes.csv")

links <- as.matrix(Trade_dyads_ab_trades)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,5])
head(adjmat)

Trade_dyads_abc<-rank(t(adjmat), ties.method = "max")
Trade_dyads_abc
##
net_adjmat <- as.network(x = adjmat, # the network object
                   directed = TRUE, # specify whether the network is directed
                   loops = FALSE, # do we allow self ties (should not allow them)
                   matrix.type = "adjacency" # the type of input
                   )

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)

# plot the network
pdf("Network_Plot_Trade.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
)
plot.network(net_adjmat,
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net_adjmat,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf

plot(net_adjmat,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))

##

##as ranked
links <- as.matrix(Trade_dyads_ab)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,3])
head(adjmat)



###############################################
################# Example 3 ###################
###############################################

# simulate network
num_nodes <- 100
edge_values <- round(runif(n = num_nodes*num_nodes , min = 1, max = 100)) *
  round(runif(n = num_nodes*num_nodes, min = 0, max = .51))
my_sociomatrix2 <- matrix(edge_values, nrow = 100, ncol = 100)
diag(my_sociomatrix2) <- 0

# create network object
net3 <- as.network(x = my_sociomatrix2, # the network object
                   directed = TRUE, # specify whether the network is directed
                   loops = FALSE, # do we allow self ties (should not allow them)
                   matrix.type = "adjacency" # the type of input
)

# set attributes
size <- round(rnorm(num_nodes,100,10))
set.vertex.attribute(net3,"Size",size)
set.edge.value(net3,"Weight",edge_values)

# summarize
summary.network(net3,print.adj = FALSE)

# plot the network
pdf("Network_Plot_3.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
)
plot.network(net3,
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net3,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf


# This dataset comes in edgelist format (advice was in adjacency matrix format)
# Read in vertex dataset
Trade_dyads <- read.csv("allyVLD.csv",stringsAsFactors=F)

# Read in edgelist
allyEL <- read.csv("allyEL.csv", stringsAsFactors=F)
allyV
# Read in contiguity
contig <- read.csv("contiguity.csv",stringsAsFactors=F,row.names=1)
contig


# (1) Initialize network
# store number of vertices
n <- nrow(Trade_dyads)
Trade_dyads_net <- network.initialize(n,dir=F)

# (2) Set vertex labels
network.vertex.names(Trade_dyads_net)  <- allyV$stateabb

# (3) Add in the edges
# Note, edgelist must match vertex labels
AllyNet[as.matrix(allyEL)]  <- 1

# (4) Store country code attribute
set.vertex.attribute(x=AllyNet,             # Network in which to store
                     "ccode",            # What to name the attribute
                     allyV$ccode)            # Values to put in

# (5) Store year attribute
set.vertex.attribute(AllyNet,"created",allyV$styear)

# (6) Store network attribute
set.network.attribute(AllyNet,"contiguous",as.matrix(contig))

# Simple plot
plot(AllyNet,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))

# Save for future use
save(AllyNet, file="AllyNet")





#######PRACTICE###########

install.packages("statnet", dependencies = TRUE) 
# Remove everything in our workspace so we can start with a clean slate:
rm(list = ls())
# Set our working directory (for me this is just my desktop)
setwd("~/Desktop")
library(statnet)
set.seed(12345)

num_nodes <- 10
my_sociomatrix <- matrix(round(runif(num_nodes*num_nodes)), # edge values
                         nrow = num_nodes, #nrow must be same as ncol
                         ncol = num_nodes)
diag(my_sociomatrix) <- 0
net <- as.network(x = my_sociomatrix, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency") # the type of input
network.vertex.names(net) <- LETTERS[1:10]
network.vertex.names(net) <- c("Susan","Rachel","Angela","Carly","Stephanie","Tom","Mike","Tony","Matt","Steven")
gender <- c(rep("Female",num_nodes/2),rep("Male",num_nodes/2))
set.vertex.attribute(net, # the name of the network object
                     "Gender", # the name we want to reference the variable by in that object
                     gender) # the value we are giving that variable

age <- round(rnorm(num_nodes,20,3))
set.vertex.attribute(net,"Age",age)

summary.network(net, # the network we want to look at
                print.adj = FALSE) # if TRUE then this will print out the whole adjacency matrix.

node_colors <- rep("",num_nodes)
for(i in 1:num_nodes){
  if(get.node.attr(net,"Gender")[i] == "Female"){
    node_colors[i] <- "lightblue"
  }else{
    node_colors[i] <- "maroon"
  }
}
print(node_colors)

pdf("Network_Plot_1.pdf", # name of pdf (need to include .pdf)
    width = 10, # width of resulting pdf in inches
    height = 10 # height of resulting pdf in inches
) 
plot.network(net, # our network object
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)
dev.off() # finishes plotting and finalizes pdf


###Construct Edgelist
num_nodes <- 40
num_edges <- 80

node_names <- rep("",num_nodes)
for(i in 1:num_nodes){
  node_names[i] <- paste("person",i,sep = "_")
}
print(node_names)
edgelist <- matrix("",nrow= num_edges,ncol = 2)
edgelist

for(i in 1:num_edges){
  edgelist[i,] <- sample(x= node_names, # the names we want to sample from
                         size = 2, # sender and receiver
                         replace = FALSE # we do not allow self edges
  ) 
}
print(edgelist)

net2 <- network.initialize(num_nodes)
network.vertex.names(net2) <- node_names
net2[as.matrix(edgelist)] <- 1
net2

income <- round(rnorm(num_nodes,mean = 50000,sd = 20000))
set.vertex.attribute(net2,"Income",income)

summary.network(net2,print.adj = FALSE)
edge_weights <- round(runif(num_edges,min = 1,max = 5))
set.edge.value(net2,"trust",edge_weights)

adjacency_matrix_2 <- net2[,]
adjacency_matrix_2

num_nodes <- length(net2$val)
node_colors <- rep("",num_nodes)
maximum <- max(get.node.attr(net2,"Income"))

for(i in 1:num_nodes){
  #' Calculate the intensity of the node color depending on the person's 
  #' relative income. 
  intensity <- round((get.node.attr(net2,"Income")[i]/maximum)*255)
  node_colors[i] <- rgb(red = 51, # the proportion of red
                        green = 51, # the proportion of green
                        blue = 153, # the proportion of blue
                        alpha = intensity, # the intensity of the color
                        max = 255 # the maximum possible intensity
  ) 
}


pdf("Network_Plot_2.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
) 
plot.network(net2, 
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = 3, # set node size to a fixed value
             displaylabels = T, # show the node names
             label.pos = 5, # display the names directly over nodes
             label.col = "yellow", # the color of node lables
             edge.lwd = get.edge.value(net2,"trust") # edge width based on trust
)
dev.off() # finishes plotting and finalizes pdf



####now weighted matrix

num_nodes <- 100
edge_values <- round(runif(n = num_nodes*num_nodes , min = 1, max = 100))*round(runif(n = num_nodes*num_nodes, min = 0, max = .51))
my_sociomatrix2 <- matrix(edge_values, nrow = 100, ncol = 100)
diag(my_sociomatrix2) <- 0
net3 <- as.network(x = my_sociomatrix2, # the network object
                   directed = TRUE, # specify whether the network is directed
                   loops = FALSE, # do we allow self ties (should not allow them)
                   matrix.type = "adjacency" # the type of input
)

size <- round(rnorm(num_nodes,100,10))
set.vertex.attribute(net3,"Size",size)
set.edge.value(net3,"Weight",edge_values)
summary.network(net3,print.adj = FALSE)

pdf("Network_Plot_3.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
) 
plot.network(net3, 
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net3,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf



####
num_nodes <- 100
install.packages("randomNames",dependencies = T)
library(randomNames)
node_names <- randomNames(num_nodes,name.order = "first.last",name.sep = "_")
length(unique(node_names)) == num_nodes
relational_information <- matrix(NA,nrow = num_nodes,ncol = num_nodes)
relational_information

max_receivers <- 0
for(i in 1:num_nodes){
  num_receivers <- min(rpois(n = 1, lambda =2),num_nodes)
  # If there are any recievers for this sender
  if(num_receivers > 0){
    receivers <- sample(x = node_names[-i], size = num_receivers, replace = F)
    relational_information[i,1:num_receivers] <- receivers
  }
  # Keep track on the maximum number of receivers
  if(num_receivers > max_receivers){
    max_receivers <- num_receivers 
  }
}

relational_information <- cbind(node_names,relational_information[,1:max_receivers])
head(relational_information)

ages <- round(rnorm(n = num_nodes, mean = 40, sd = 5))
genders <- sample(c("Male","Female"),size = num_nodes, replace = T)
node_level_data <- cbind(node_names,ages,genders)

node_level_data <- node_level_data[sample(x = 1:num_nodes, size = num_nodes-10),]

write.table(x = node_level_data, 
            file = "node_level_data.csv",
            sep = ",",
            row.names = F)
write.table(x = relational_information, 
            file = "relational_information.csv",
            sep = ",",
            row.names = F)
edge_matrix <- read.csv(file = "relational_information.csv",
                        sep = ",",
                        stringsAsFactors = F, # Always include this argument!
                        header = T)
node_covariates <- read.csv(file = "node_level_data.csv",
                            sep = ",",
                            stringsAsFactors = F, # Always include this argument!
                            header = T)
nrow(edge_matrix) == nrow(node_covariates)


Process_Node_and_Relational_Data <- function(node_level_data,
                                             relational_data,
                                             node_id_column = 1){
  #get our node ids
  node_ids <- node_level_data[,node_id_column]
  #remove any missing or blank entries
  to_remove <- which(node_ids == "" | is.na(node_ids))
  if(length(to_remove) > 0){
    node_ids <- node_ids[-to_remove]
    node_level_data <- node_level_data[-to_remove,]
  }
  # Allocate a blank edgelist to return
  edgelist <- NULL
  # Loop over rows to check them
  for(i in 1:length(relational_data[,1])){
    # Check to see if the sender is in the dataset
    if(length(which(node_ids == relational_data[i,1]) > 0)){
      #' If we have a valid sender, check to see if there is a valid reciever 
      #' and add them to the dataset if they are valid as well for each 
      #' receiver
      for(j in 2:ncol(relational_data)){
        if(!is.na(relational_data[i,j])){
          if(length(which(node_ids == relational_data[i,j]) > 0)){
            edge <- c(relational_data[i,1],relational_data[i,j])
            edgelist <- rbind(edgelist,edge)
          }
        }
      }
    }
  }
  # Give column names to edgelist
  colnames(edgelist) <- c("sender","receiver")
  # Return cleaned data as a list object
  return(list(node_level_data = node_level_data, 
              edgelist = edgelist,
              num_nodes = length(node_level_data[,1]),
              node_names = node_ids))
}
Clean_Data <- Process_Node_and_Relational_Data(node_level_data = node_covariates, 
                                                 relational_data = edge_matrix,
                                                 node_id_column = 1)
net4 <- network.initialize(Clean_Data$num_nodes)


network.vertex.names(net4) <- Clean_Data$node_names
net4[as.matrix(Clean_Data$edgelist)] <- 1
set.vertex.attribute(net4,"Age",Clean_Data$node_level_data$ages)
set.vertex.attribute(net4,"Gender",Clean_Data$node_level_data$genders)
summary.network(net4,print.adj = FALSE)

######
#####
####
###
###
Trade partners over a preselected value

####
links <- read.csv("edges.csv")
nodes <- read.csv("limited nodes.csv")

links1 <- as.matrix(Trade_dyads_abcd)
labels1 <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels1) # put into alphabetical order

adjmat1 <- matrix(0, length(labels1), length(labels1))
colnames(adjmat1) <- labels1
rownames(adjmat1) <- labels1
adjmat1[links1[,1:2]] <- as.numeric(links1[,3])
head(adjmat1)

Trade_dyads_abc<-rank(t(adjmat), ties.method = "max")
Trade_dyads_abc
##
net_adjmat1 <- as.network(x = adjmat, # the network object
                         directed = TRUE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)

# plot the network
pdf("Network_Plot_Trade.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
)
plot.network(net_adjmat,
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net_adjmat,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf

plot(net_adjmat,displaylabels=T,label.cex=.3,edge.col=rgb(150,150,150,100,maxColorValue=255))

#
#
#
#
#
#
#
#
Trade_dyads_ab_trades
links <- as.matrix(Trade_dyads_ab_trades)
links
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,6])
head(adjmat)

Trade_dyads_abc<-rank(t(adjmat), ties.method = "max")
Trade_dyads_abc
##
net_adjmat <- as.network(x = adjmat, # the network object
                         directed = TRUE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat1[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)

# plot the network
pdf("Network_Plot_Trade.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
)
plot.network(net_adjmat,
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net_adjmat,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf

plot(net_adjmat,displaylabels=F,label.cex=.4,edge.col=rgb(150,150,150,100,maxColorValue=255))


















#####
#####
#####
#Let's create some likeness scores. . . main idea: do for population, then for gdp per cap 
#gives pop_like * econ_like
dyads_edgelisting<-Trade_dyads_ab[ ,c(1,2)]
international_economic_data<-read.csv("~/GitHub/SSHRC_Replication_File/international_economy.csv", header = TRUE)
names(international_economic_data)[1]<-paste("importer1")
international_economic_data<-international_economic_data[international_economic_data$Time==2006,]
head(Trade_dyads_abc)
Trade_dyads_abc <- merge(dyads_edgelisting, international_economic_data, by= c("importer1"))
names(international_economic_data)[1]<-paste("importer2")
Trade_dyads_abcd <- merge(Trade_dyads_abc, international_economic_data, by= c("importer2"))
head(Trade_dyads_abcd)
#subset 1
#population
Trade_dyads_abcd$Pop1 <-as.numeric(gsub(",", "", Trade_dyads_abcd$Population..total..SP.POP.TOTL..x))
Trade_dyads_abcd$Pop2 <-as.numeric(gsub(",", "", Trade_dyads_abcd$Population..total..SP.POP.TOTL..y))

#
Trade_dyads_abcd$GDP_cap1 <-as.numeric(gsub(",", "", Trade_dyads_abcd$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD..x))
Trade_dyads_abcd$GDP_cap2 <-as.numeric(gsub(",", "", Trade_dyads_abcd$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD..y))


Pop_likeness<-Trade_dyads_abcd[,c("importer1", "importer2", "Pop", "Pop2")]
likeness_scoring<-Trade_dyads_abcd[,c("importer1", "importer2", "Pop", "Pop2", "GDP_cap1", "GDP_cap2")]
likeness_scoring$GDP_cap_likeness<-likeness_scoring$GDP_cap1/likeness_scoring$GDP_cap2

likeness_scoring$Pop_cap_likeness<-likeness_scoring$Pop/likeness_scoring$Pop2


likeness_scoring<-na.omit(likeness_scoring)
likeness_scoring$Pop_cap_likeness[likeness_scoring$Pop_cap_likeness>1]<-(likeness_scoring$Pop_cap_likeness[likeness_scoring$Pop_cap_likeness>1])^(-1)
likeness_scoring$Pop_cap_likeness[likeness_scoring$Pop_cap_likeness>0.9]<-1
likeness_scoring$Pop_cap_likeness[likeness_scoring$Pop_cap_likeness<=0.9]<-0


likeness_scoring$GDP_cap_likeness[likeness_scoring$GDP_cap_likeness>1]<-(likeness_scoring$GDP_cap_likeness[likeness_scoring$GDP_cap_likeness>1])^(-1)
likeness_scoring$GDP_cap_likeness[likeness_scoring$GDP_cap_likeness>0.9]<-1
likeness_scoring$GDP_cap_likeness[likeness_scoring$GDP_cap_likeness<=0.9]<-0


likeness_scoring$likeness_score<-likeness_scoring$Pop_cap_likeness*likeness_scoring$GDP_cap_likeness
likeness_scoring$likeness_score[likeness_scoring$likeness_score>0.2]<-1
likeness_scoring$likeness_score[likeness_scoring$likeness_score<=0.2]<-0




overal_likeness_scoring<-likeness_scoring[,c("importer1", "importer2", "likeness_score")]

links <- as.matrix(overal_likeness_scoring)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,3])
head(adjmat)

pop_similarity_network<-rank(t(adjmat), ties.method = "max")

##
net_adjmat <- as.network(x = adjmat, # the network object
                         directed = FALSE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)


plot(net_adjmat,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))








population__likeness_scoring<-likeness_scoring[,c("importer1", "importer2", "Pop_cap_likeness")]

links <- as.matrix(population__likeness_scoring)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,3])
head(adjmat)

pop_similarity_network<-rank(t(adjmat), ties.method = "max")

##
net_adjmat <- as.network(x = adjmat, # the network object
                         directed = FALSE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)


plot(net_adjmat,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))






GDP_capital_likeness<-likeness_scoring[,c("importer1", "importer2", "GDP_cap_likeness")]

links <- as.matrix(GDP_capital_likeness)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,3])
head(adjmat)

pop_similarity_network<-rank(t(adjmat), ties.method = "max")

##
net_adjmat <- as.network(x = adjmat, # the network object
                         directed = FALSE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)


plot(net_adjmat,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))




mean(Pop_likeness$Pop)
Pop_likeness$Pop_Rat<-Pop_likeness$Pop/Pop_likeness$Pop2
Pop_likeness[Pop_likeness$importer1=="Canada",]
Pop_likeness$Pop_Rat[Pop_likeness$Pop_Rat>1]<-(Pop_likeness$Pop_Rat[Pop_likeness$Pop_Rat>1])^(-1)
head(Pop_likeness)
Pop_likeness_edges<-Pop_likeness[,c("importer1", "importer2", "Pop_cap_likeness")]


links <- as.matrix(Pop_likeness_edges)
labels <- unique(c(links[,1], links[,2]))
head(links)
ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- labels
rownames(adjmat) <- labels
adjmat[links[,1:2]] <- as.numeric(links[,3])
head(adjmat)

pop_similarity_network<-rank(t(adjmat), ties.method = "max")

##
net_adjmat <- as.network(x = adjmat, # the network object
                         directed = FALSE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency" # the type of input
)

# set attributes
#size <- round(rnorm(num_nodes,100,10))
#set.vertex.attribute(net3,"Size",size)
set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat,print.adj = FALSE)

# plot the network
pdf("Network_Plot_Trade.pdf", # name of pdf (need to include .pdf)
    width = 20, # width of resulting pdf in inches
    height = 20 # height of resulting pdf in inches
)
plot.network(net_adjmat,
             vertex.col = "purple", #just one color
             displaylabels = F, # no node names
             edge.lwd = log(get.edge.value(net_adjmat,"Weight")) # edge width
)
dev.off() # finishes plotting and finalizes pdf

plot(net_adjmat,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))
