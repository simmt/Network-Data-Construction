soc_tax_data_tnam<-na.omit(international_tax_data[,c("ISO", 
                                                     "Year",
                                                     "Social_Security")])
corp_tax_data_tnam<-international_tax_data[,c("ISO", 
                                              "Year",
                                              "Corporate")]
gdp_data_tnam_sim1<-gdp_data_tnam_sim1[,c("ISO", 
                                          "Year",
                                          "GDP_cap")]
polity_data_tnam<-polity[,c("ISO", "Year","polity2")]
WTO_tnam<-WTO_mem[,c("ISO", "Year", "1990":"2015")]
WTO_tnam<-WTO_tnam[,-2]
?reshape
WTO_tnam_long<-reshape(WTO_tnam, idvar = "ISO", varying = c(2:ncol(WTO_tnam)),
        timevar = "Year", v.names = "WTO", direction = "long")

library(reshape2)
long <- melt(na.omit(WTO_tnam), id.vars = c("ISO"))
colnames(long)[2] <- "Year"



?merge
updated_file<-merge(soc_tax_data_tnam, corp_tax_data_tnam, by = c("ISO", "Year"), all = TRUE)
updated_file<-merge(updated_file, gdp_data_tnam_sim1, by = c("ISO", "Year"), all = TRUE)
updated_file<-merge(updated_file, polity_data_tnam, by = c("ISO", "Year"), all = TRUE)
updated_file<-merge(updated_file, long, by = c("ISO", "Year"), all = TRUE)
updated_file$Year<-as.factor(updated_file$Year)
updated_file$ISO<-as.factor(updated_file$ISO)
colnames(updated_file)[7] <- "superduper"
updated_file$superduper<-as.factor(updated_file$superduper)
updated_file<-na.omit(updated_file)
updated_file$GDP_cap_log<-updated_file$GDP_cap

updated_file
head(updated_file)
boxplot(Social_Security ~ Year, data = updated_file, ylim = c(0, 20), xlim = c(1990, 2005))
library(ggplot2)
library(gplots)
plotmeans(Social_Security ~ Year, data = updated_file, subset = superduper=="1")
plotmeans(Social_Security ~ Year, data = updated_file, subset = superduper=="0")

library(lfe)
updated_file_lim<-updated_file[updated_file$Year>1989 & updated_file$Year<2001,]
updated_file_lim
est <- felm(Social_Security ~  GDP_cap_log+superduper| ISO, data = updated_file_lim)
est <- felm(Corporate ~  GDP_cap_log+superduper| ISO, data = updated_file)
updated_file
summary(est)
est$
head(updated_file)

?felm
summary(est)
?plotmeans
p <- ggplot(updated_file, aes(Year, Corporate, group = Year))
p + geom_boxplot() + scale_x_discrete(limits = c(1989, 2001), drop=TRUE)
?scale_x_discrete
plot(Social_Security ~ Year, data = updated_file, xlim = c(1990, 2010))
??fe
felm(Social_Security ~ Year, data = updated_file)
library(lfe)
felm(Social_Security ~ dat$x1 + G(dat$x2) + G(dat$x3))
library(plm)
?plm

pmodel2 <- plm(Social_Security ~ as.factor(superduper), data=updated_file, 
               index=c("ISO", "Year"), model="within")
