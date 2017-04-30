library(countrycode)
Trade_dyads<-read.csv("~/Downloads/COW_Trade_3.0/dyadic_trade_3.0.csv",stringsAsFactors=F)
Trade_dyads_i<-Trade_dyads[Trade_dyads$year==2000,]
Trade_dyads_a<-Trade_dyads_i[,c("importer1", "importer2", "flow1")]
Trade_dyads_b<-Trade_dyads_i[,c("importer2", "importer1", "flow2")]
names(Trade_dyads_b) <- c("importer1", "importer2", "flow1")
Trade_dyads_ab<-rbind(Trade_dyads_a, Trade_dyads_b)
Trade_dyads_ab$importer1<-as.factor(countrycode(Trade_dyads_ab$importer1, "country.name",  "cowc"))
Trade_dyads_ab$importer2<-as.factor(countrycode(Trade_dyads_ab$importer2, "country.name",  "cowc"))

#try to do as rank variable, keep top 3

library(data.table)
one_Trade_dyads_ab_trades <- as.data.table(Trade_dyads_ab)

####now get top importers
one_Trade_dyads_ab_trades[,tr_rank:=rank(-flow1,ties.method="first"),by=importer1]
###
one_Trade_dyads_ab_trades[,tr_rank_three := tr_rank>0 & tr_rank<3]
one_Trade_dyads_ab_trades$tr_rank_three[one_Trade_dyads_ab_trades$tr_rank_three=="TRUE"]<-1
one_Trade_dyads_ab_trades$tr_rank_three[one_Trade_dyads_ab_trades$tr_rank_three=="FALSE"]<-0


###also, now get in some world trade org data
WTO_mem<-read.csv("~/Downloads/wto_mem.csv",stringsAsFactors=F)
library(stringr)
WTO_mem$Year<-as.numeric(str_sub(WTO_mem$WTO.Accession.Date, -4, -1))
WTO_mem$Year[is.na(WTO_mem$Year)]<-9999
names(WTO_mem)[names(WTO_mem)=="Country.Name"] <- "importer1"
WTO_mem$importer1<-as.factor(countrycode(WTO_mem$importer1, "country.name",  "cowc"))

Trade_dyads_abc_trades<-merge(one_Trade_dyads_ab_trades, WTO_mem, by=c("importer1"), all = FALSE)
Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year==2000]<-1
Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year>1]<-0

links <- as.matrix(one_Trade_dyads_ab_trades)
labels <- unique(c(links[,1], links[,2]))

ordering <- sort(labels) # put into alphabetical order

adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- ordering
rownames(adjmat) <- ordering
adjmat[links[,1:2]] <- as.numeric(links[,5])

###covariate....WTO member
cov_ergm<-data.frame("importer1" = ordering, NA)
cov_ergm<-merge(cov_ergm, WTO_mem, by=c("importer1"), all.x = TRUE, all.y = FALSE)

cov_ergm1<-data.frame("importer1" = cov_ergm$importer1, "covar" = cov_ergm$Year)
cov_ergm1$covar[is.na(cov_ergm1$covar)]<-0
cov_ergm1$covar[cov_ergm1$covar<2000]<-1
cov_ergm1$covar[cov_ergm1$covar>1]<-0

##
library(statnet)
net_adjmat1 <- as.network(x = adjmat, # the network object
                         directed = TRUE, # specify whether the network is directed
                         loops = FALSE, # do we allow self ties (should not allow them)
                         matrix.type = "adjacency") # the type of input

# set attributes
#size <- round(rnorm(num_nodes,100,10))
set.vertex.attribute(net_adjmat1,"covar", cov_ergm1$covar)
#set.edge.value(net_adjmat,"Weight",net_adjmat[,3])

# summarize
summary.network(net_adjmat1,print.adj = FALSE)

# plot the network
plot.network(net_adjmat1,
             vertex.col = "purple", #just one color
             displaylabels = F) # no node names

#edge.lwd = log(get.edge.value(net_adjmat,"Weight"))) # edge width

plot(net_adjmat1,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))
