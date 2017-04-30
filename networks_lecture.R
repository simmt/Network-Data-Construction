library(GERGM)
# game data accessed at spreadsheetsports.com
# https://www.spreadsheetsports.com/2015-ncaa-basketball-game-data

gameData <- read.csv("~/Downloads/GERGM Lab/GameResults2016.csv",stringsAsFactors=F)

allTeams <- sort(unique(gameData$Team))

# ACC Men's basketball
accTeams <- c("Boston College","Clemson","Duke","Florida State","Georgia Tech","Louisville","Miami (FL)","North Carolina","North Carolina State","Notre Dame","Pittsburgh","Syracuse","Virginia","Virginia Tech","Wake Forest")

# Pulled off the wikipedia site
enrollment <- c(14500,17165,6247,38886,19393,23262,15520,26878,29957,11733,28823,20407,20399,28000,6451)/1000


ACCGameData <- subset(gameData,is.element(Team,accTeams) & is.element(Opponent, accTeams))

adjacencyMatrix <- matrix(0,length(accTeams),length(accTeams))

plot_network(adjacencyMatrix)

rownames(adjacencyMatrix) <- accTeams

colnames(adjacencyMatrix) <- accTeams

nGames <- matrix(0,length(accTeams),length(accTeams))

for(i in 1:nrow(ACCGameData)){
  sender <- match(ACCGameData$Team[i],accTeams)
  receiver <- match(ACCGameData$Opponent[i],accTeams)
  score <- ACCGameData$Team.Score[i]
  adjacencyMatrix[sender,receiver] <- adjacencyMatrix[sender,receiver] + score
  nGames[sender,receiver] <- nGames[sender,receiver] + 1
}

adjacencyMatrix <- adjacencyMatrix/100

covariateData <- data.frame(accTeams,enrollment)
rownames(covariateData) <- accTeams

save(list=c("adjacencyMatrix","nGames","covariateData"),file="ACCBasketball.RData")

# Thinning doesn't work
# 'transforming netowrks' fit step at the end takes 100+ times longer than inference
# Distributional families for lambda appear not to work




# Read in R package for GERGM
library(GERGM)

# Load data objects
# data covers NCAA ACC Men's basketball from the 2016 season
# adjacencyMatrix is the number of points i scored on j during 2016 season (in 100s)
# nGames is the number of games played between i and j during the 2016 season
# covariateData contains team name and school enrollment (in thousands)
load("ACCBasketball.RData")

# Specify the formula to include the desired effects
formula <- adjacencyMatrix ~ sender("enrollment") +receiver("enrollment") +netcov(nGames)+mutual(alpha=.9) + edges

# Set the random number seed to assure replicability
set.seed(5)

# Run the gergm
gergmResults <- gergm(formula, # use the formula we defined
                      covariate_data=covariateData, # point to the covariate data
                      number_of_networks_to_simulate=100000, # large value = accuracy
                      MCMC_burnin=10000, # large value speeds convergence
                      thin=1/10, # don't keep all of the simulated networks
                      transformation_type="Cauchy") # specify a distribution for g()

Trace_Plot(gergmResults)
Estimate_Plot(gergmResults)
# Extract matrix of estimates and standard errors
EstSE <- rbind(t(attributes(gergmResults)$theta.coef),t(attributes(gergmResults)$lambda.coef))

# Create a LaTeX table
library(xtable)
xtable(EstSE,dig=4)

# Plot and evaluate goodness of fit
pdf(file="GOFPlot.pdf",height=4,width=8)
GOF(gergmResults)
dev.off()

# Save results
save(list="gergmResults",file="gergmResults.RData")



##########
##########

## =============================================================================
## Description: R examples and code for Chapter 10
## =============================================================================

library(latentnet)
load('~/Downloads/Strike.RData')
rm(Strike)

Strike<-strike

set.seed(10)
lab <- network.vertex.names(Strike)
col <- Strike %v% "group"
plot(Strike, vertex.cex=1.5, label=lab, edge.col="gray", vertex.col=col,
     vertex.border=col, pad=0.1)


## Estimate the 2d latent space model for the strike communication network,
## setting the seed for replicability.     
strike.2d <- ergmm(Strike ~ euclidean(d=2), verbose=TRUE)

plot(strike.2d, vertex.col=col, vertex.border=col, plot.vars=FALSE,
     pad=0)

## Plot some diagnostics for convergence.     
mcmc.diagnostics(strike.2d, which.diags="trace")
mcmc.diagnostics(strike.2d, which.diags="acf")

## Simulate edges and triangles, check against observed.
o <- summary(Strike ~ edges + triangle)
sims <- simulate.ergmm(strike.2d, nsim=2500)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)












mod <- ergmm(Strike ~ 1(mean=1) + euclidean(d=2), verbose=TRUE,
             control=ergmm.control(burnin=20000, interval=100))
plot.ergmm(mod, vertex.col=col, vertex.border=col, plot.vars=FALSE,
      pad=0)
sims <- simulate.ergmm(mod, nsim=1000)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)









strike.test <- ergmm(strike ~ euclidean(d=2, mean.var=1), seed=5, verbose=TRUE)

o <- summary(strike ~ edges + triangle)
sims <- simulate.ergmm(strike.test, nsim=1000)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)









strike.1d <- ergmm(strike ~ euclidean(d=1), seed=5, verbose=TRUE)
o <- summary(strike ~ edges + triangle)
sims <- simulate.ergmm(strike.1d, nsim=2500)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)


strike.3d <- ergmm(strike ~ euclidean(d=3), seed=5, verbose=TRUE)
o <- summary(strike ~ edges + triangle)
sims <- simulate.ergmm(strike.3d, nsim=2500)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)









strike.bl <- ergmm(strike ~ bilinear(d=2), seed=5, verbose=TRUE)

o <- summary(strike ~ triangle)
sims <- simulate.ergmm(strike.bl, nsim=500)
s <- sapply(sims$networks, function(x) summary(x ~ triangle))

hist(s, main=NULL, breaks="FD")
abline(v=quantile(s, probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o, col="red", lty=2, lwd=2)

plot(strike.bl)



strike %v% "group_factor" <- paste0("g", strike %v% "group")
strike.grp <- ergmm(strike ~ euclidean(d=2) + nodematch("group_factor"), verbose=TRUE)

o <- summary(strike ~ edges + triangle)
sims <- simulate.ergmm(strike.grp, nsim=1000)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)



lang <- ifelse(strike %v% "group" == 1, "Spanish", "English")
lang[strike %v% "vertex.names" %in% c("Alejandro", "Bob")] <- "Both"
strike %v% "language" <- lang

strike.lang <- ergmm(strike ~ euclidean(d=2) + nodematch("language"), verbose=TRUE)
summary(strike.lang)
plot(strike.lang)

o <- summary(strike ~ edges + triangle)
sims <- simulate.ergmm(strike.lang, nsim=1000)
s <- t(sapply(sims$networks, function(x) summary(x ~ edges + triangle)))

par(mfrow=c(1,2))
hist(s[,1], main="edges", breaks="FD")
abline(v=quantile(s[,1], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[1], col="red", lty=2, lwd=2)

hist(s[,2], main="triangles", breaks="FD")
abline(v=quantile(s[,2], probs=c(0.025, 0.975)), col="blue", lty=2, lwd=2)
abline(v=o[2], col="red", lty=2, lwd=2)


plot(strike.lang)


###tnam



#
#
#

library("tnam")
library("texreg")
rm(list = ls())
set.seed(123)
data("knecht")
dim(friendship[[1]])
delinquency <- as.data.frame(delinquency)
rownames(delinquency) <- letters
delinquency

friendship[[3]][friendship[[3]] == 10] <- NA
friendship[[4]][friendship[[4]] == 10] <- NA
for (i in 1:length(friendship)) {
  rownames(friendship[[i]]) <- letters
}


sex <- demographics$sex
names(sex) <- letters
sex <- list(t1 = sex, t2 = sex, t3 = sex, t4 = sex)

religion <- demographics$religion
names(religion) <- letters
religion <- list(t1 = religion, t2 = religion, t3 = religion, t4 = religion)

delinquency
model1 <- tnam(
  delinquency ~
    covariate(sex, coefname = "sex") +
    covariate(religion, coefname = "religion") +
    covariate(delinquency, lag = 1, exponent = 1) +
    netlag(delinquency, friendship) +
    netlag(delinquency, friendship, pathdist = 2, decay = 1) +
    netlag(delinquency, friendship, lag = 1) +
    degreedummy(friendship, deg = 0, reverse = TRUE) +
    centrality(friendship, type = "betweenness"),
  re.node = TRUE, time.linear = TRUE
)
?netlag
screenreg(model1, single.row = TRUE)


nl <- netlag(delinquency, friendship, pathdist = 2, decay = 1)
head(nl)


?tnam
?"tnam-terms"


