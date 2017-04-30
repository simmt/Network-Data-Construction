hga <- list(net_adjmat, net_adjmat1, net_adjmat2) # create list from oldest to newest. This will be the outcome object for TERGM analysis

## Plot the network
set.seed(5)
par(mfrow = c(1,1))
hgat <- c("1995", "2000", "2005")  
for (i in 1:length(hga)){
  plot(hga[[i]],vertex.cex=1,label.cex=.5,displaylabels=T,
       edge.col=rgb(150,150,150,100,maxColorValue=255),
       label.pos=5, main=hgat[[i]])}

# TERGM Analysis

library(xergm)

# Basic model
set.seed(12345)
?btergm
?degcor
m1 <- btergm(hga ~ edges 
             + mutual
             #+nodemix('covar')
             + nodematch('covar', diff = TRUE)
             +gwdsp(1, fixed=TRUE)
             #+degree(1)
             #+degcor
             #+istar(2)
             #+triangles
             #+ ttriple 
             #+ transitiveties 
             #+ ctriple 
             #+ gwesp(0.5, fixed=TRUE)
             #+ cycle(4)
             #+ idegreepopularity
             #+ odegreepopularity
             #+ istar(2)
             #+ ostar(2)
             #+ nodematch("female", diff=TRUE) 
             #+ absdiff("covar")
             #+ nodemix("female", base=c(1,4))
             #+ nodefactor("covar")
             #+ delrecip
             + memory(type="stability"),
              R=10000)
summary(m1)
library(texreg)
texreg(m1)
sum_m1<-as.matrix(m1)
gof1 <- gof(m1, statistics = c(esp, dsp, geodesic,deg, triad.undirected, walktrap.modularity))
plot(gof1)
