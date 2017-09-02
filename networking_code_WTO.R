######DATA PREPERATION######
library(foreign)
library(reshape2)

######Tax data
international_tax_data<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
names(international_tax_data)[names(international_tax_data)=="Calendar.year..nearest."] <- "Year"

#Likeness & Covariates#
######GDP per cap, Population, 
international_econ_data<-read.csv("~/Desktop/Network Data Construction/Data_Extract_From_World_Development_Indicators (5) 2/country_stats.csv")

######Polity
##add polity score
polity<-read.csv("~/Desktop/Network Data Construction/Polity/p4v2014.csv")
library(plyr)
polity<-rename(polity, c(year="Year", country="Country"))
#polity$Country<-revalue(polity$Country, c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic", "Korea South"="Korea, Rep.",
#                                          "Iran"="Iran, Islamic Rep.", "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR", "Egypt"="Egypt, Arab Rep.",
#                                          "Venezuela"="Venezuela, RB"))
#end likeness
#
#####WTO Member, GATT
###also, now get in some world trade org data
WTO_mem<-read.csv("~/Desktop/Network Data Construction/wto_mem.csv",stringsAsFactors=F)
library(stringr)
WTO_mem$Year<-as.numeric(str_sub(WTO_mem$WTO.Accession.Date, -4, -1))
WTO_mem$Year[is.na(WTO_mem$Year)]<-9999
names(WTO_mem)[names(WTO_mem)=="Country.Name"] <- "importer1a"
library(countrycode)
WTO_mem$importer1<-as.factor(countrycode(WTO_mem$importer1a, "country.name",  "cowc"))
WTO_mem

#now establish data in form needed
######DVs..... Tax data#######
international_tax_data$Social_Security<-as.numeric(sub("%", "", international_tax_data$Social.contributions))
#international_tax_data$Corp<-as.numeric(sub("%", "", international_tax_data$Corp))
international_tax_data$Corporate<-as.numeric(sub("%", "", international_tax_data$Corporations.and.other.enterprises))


soc_tax_data_tnam<-na.omit(international_tax_data[,c("ISO", 
                                         "Year",
                                         "Social_Security")])
corp_tax_data_tnam<-international_tax_data[,c("ISO", 
                                             "Year",
                                             "Corporate")]
soc_tax_data_tnam$ISO<-as.character(soc_tax_data_tnam$ISO)
soc_tax_data_tnam$Year<-as.character(soc_tax_data_tnam$Year)
corp_tax_data_tnam$ISO<-as.character(corp_tax_data_tnam$ISO)
corp_tax_data_tnam$Year<-as.character(corp_tax_data_tnam$Year)

library(reshape2)

soc_tax_tnam_w <- dcast(soc_tax_data_tnam, ISO ~ Year, value.var="Social_Security")
soc_tax_tnam_ww<- data.frame(soc_tax_tnam_w[,2:ncol(soc_tax_tnam_w)], row.names = soc_tax_tnam_w$ISO)

summary(tnam(soc_tax_tnam_ww~covariate(soc_tax_tnam_ww, lag = 1), re.node = T))

head(soc_tax_tnam_w)
str(soc_tax_tnam_w)
soc_tax_2000<-soc_tax_data_tnam[soc_tax_data_tnam$Year=="2000",]
soc_tax_2001<-soc_tax_data_tnam[soc_tax_data_tnam$Year=="2001",]
soc_tax_2002<-soc_tax_data_tnam[soc_tax_data_tnam$Year=="2002",]
?merge
soc_tax_three<-merge(soc_tax_2000$Social_Security, soc_tax_2001$Social_Security, 
                 soc_tax_2002$Social_Security)
soc_two<-merge(soc_tax_2000, soc_tax_2001, by = c("ISO"))
soc_three<-merge(soc_two, soc_tax_2002, by = c("ISO"))
head(soc_three)
str(soc_three_a)
soc_three_a<-soc_three[, c("Social_Security.x", "Social_Security.y",
                           "Social_Security")]
rownames(soc_three_a)<-soc_three$ISO
soc_three_a<-as.data.frame(soc_three_a)
soc_tax_tnam_www<-data.frame(soc_tax_tnam_w[,2:5])
rownames(soc_tax_tnam_www)<-soc_tax_tnam_w$ISO
soc_tax_tnam_www<-tnamdata(soc_tax_tnam_www~1)
str(soc_tax_tnam_www) 
library(tnam)
trialcasesoftnammy <- tnam(soc_three_a ~ 1)


                             
social_securitys_listing2<-cbind(soc_tax_tnam_w$`2000`, soc_tax_tnam_w$`2001`, soc_tax_tnam_w$`2002`)
sslist<-soc_tax_tnam_w


sslist1<-as.numeric(as.character(soc_tax_tnam_w$`2000`))
names(sslist1)<-soc_tax_tnam_w$ISO

sslist2<-as.numeric(as.character(soc_tax_tnam_w$`2001`))
names(sslist2)<-soc_tax_tnam_w$ISO

sslist3<-as.numeric(as.character(soc_tax_tnam_w$`2002`))
names(sslist3)<-soc_tax_tnam_w$ISO

social_securitys_listing2<-list(t1=sslist1, t2=sslist2, t3=sslist3)
social_securitys_listing2<-as.data.frame(social_securitys_listing2)
social_securitys_listing2
trialcasesoftnammy <- tnam(social_securitys_listing2 ~ covariate(social_securitys_listing2, lag = 1))
summary(trialcasesoftnammy)



soc_tax_tnam_w <- dcast(soc_tax_data_tnam, ISO ~ Year)
head(soc_tax_tnam_w)
str(soc_tax_tnam_w)
library(dplyr)
ncol(soc_tax_tnam_w)
links_i <- as.matrix(soc_tax_tnam_w)
labels_i <- unique(c(links_i[,1]))
ordering_i <- sort(labels_i) # put into alphabetical order
adjmat_i <- matrix(0, length(labels_i), (ncol(soc_tax_tnam_w)-1))
rownames(adjmat_i) <- ordering_i
#colnames(adjmat_i) <- names(soc_tax_tnam_w[,2:ncol(soc_tax_tnam_w)])
colnames(adjmat_i) <- as.character(colnames(links_i[,2:ncol(links_i)]))
adjmat_i[,1:ncol(adjmat_i)] <- as.numeric(links_i[,2:ncol(links_i)])

head(adjmat_i)
str(adjmat_i)
trialcasesoftnammy <- tnam(adjmat_i ~ 1)



soc_tax_tnam_w_xxx<-select(soc_tax_tnam_w, -c("NA"))

head(soc_tax_tnam_w)
#note, added as.data.frame
?dcast
soc_tax_data_tnam$Social_Security<-as.numeric(as.character(soc_tax_data_tnam$Social_Security))
head(soc_tax_tnam_w)
#note changing structures....class(soc_tax_tnam_w)
soc_tax_tnam_w_test<-cbind(soc_tax_tnam_w$`2000`, soc_tax_tnam_w$`2001`, soc_tax_tnam_w$`2002`, 
                           soc_tax_tnam_w$`2003`, soc_tax_tnam_w$`2004`)
soc_tax_tnam_w_test<-as.data.frame(soc_tax_tnam_w_test)
rownames(soc_tax_tnam_w_test)<-soc_tax_tnam_w$ISO
soc_tax_tnam_w_test
?dcast
trialcasesoftnammy <- tnam(social_securitys_listing2 ~ 1)
trialcasesoftnammy

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
summary(model1)

soc_tax_tnam_w_test <- soc_tax_tnam_w[ , c("2002", "2003", "2004", "2005")]


soc_tax_tnam_w_test <- data.matrix(soc_tax_tnam_w_test)
soc_tax_tnam_w_test<-as.numeric(as.character(soc_tax_tnam_w_test[1:4,]))
head(soc_tax_tnam_w_test)
names_for_shits<-soc_tax_tnam_w[,"ISO"]
rownames(soc_tax_tnam_w_test) <- names_for_shits
names_for_shits1<-c("t1", "t2", "t3", "t4")
colnames(soc_tax_tnam_w_test) <- names_for_shits1

soc_tax_tnam_w_test_df<-data.frame(soc_tax_tnam_w_test$t1, soc_tax_tnam_w_test$t2,
                                   soc_tax_tnam_w_test$t3, soc_tax_tnam_w_test$t4)
rownames(soc_tax_tnam_w_test_df)<-soc_tax_tnam_w[,"ISO"]
head(soc_tax_tnam_w_test_df)
soc_tax_tnam_w_test
soc_tax_tnam_w_test1=as.data.frame(soc_tax_tnam_w_test)
soc_tax_tnam_w_test
head(soc_tax_tnam_w_test)
soc_tax_tnam_w_test1
<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
summary(lm(soc_tax_tnam_w_test.t4~soc_tax_tnam_w_test.t3, data = soc_tax_tnam_w_test_df))
testing_numbs<-read.csv("~/Desktop/testing_numbs.csv", header = TRUE)
testing_numbs<-testing_numbs[,1:3]
testing_numbs<-as.

?"tnamdata"
class(testing_numbs$t2000)
library(xergm)
library(tnam)
??dcast.data.table

?dcast

class(soc_tax_tnam_w_test$t2)
soc_tax_tnam_w_test<-as.data.frame(soc_tax_tnam_w_test)

model1 <- tnam(soc_tax_tnam_w_test_df ~ 1)
length(soc_tax_tnam_w_test$`2003`)

corp_tax_tnam_w <- dcast(corp_tax_data_tnam, ISO ~ Year, value.var="Corporate")

######Onto IVs as covariates#######
international_econ_data$GDP_cap<-as.numeric(international_econ_data$GDP.per.capita..constant.2010.US....NY.GDP.PCAP.KD.)
international_econ_data$Pop<-international_econ_data$Population..total..SP.POP.TOTL.
international_econ_data$GINI<-international_econ_data$GINI.index..World.Bank.estimate...SI.POV.GINI.
international_econ_data$ISO<-international_econ_data$Country.Code
international_econ_data$Year<-international_econ_data$Time

gdp_data_tnam<-international_econ_data[,c("ISO", 
                                             "Year",
                                             "GDP_cap")]
pop_data_tnam<-international_econ_data[,c("ISO", 
                                              "Year",
                                              "Pop")]
gini_data_tnam<-international_econ_data[,c("ISO", 
                                         "Year",
                                         "GINI")]

gdp_tnam <- dcast(gdp_data_tnam, ISO ~ Year, value.var="GDP_cap", fun.aggregate = median)
pop_tnam <- dcast(pop_data_tnam, ISO ~ Year, value.var="Pop", fun.aggregate = median)
gini_tnam <- dcast(gini_data_tnam, ISO ~ Year, value.var="GINI", fun.aggregate = median)


####now from polity
polity$ISO<-polity$scode
polity_data_tnam<-polity[,c("ISO", "Year","polity2")]
polity_tnam <- dcast(polity_data_tnam, ISO ~ Year, value.var="polity2")

####FORCING VAR....can later make this a proper time series
WTO_mem$ISO<-WTO_mem$importer1
WTO_mem$Year[WTO_mem$Year=="9999"]<-NA

WTO_mem$"1990"<-0
WTO_mem$"1991"<-0
WTO_mem$"1992"<-0
WTO_mem$"1993"<-0
WTO_mem$"1994"<-0
WTO_mem$"1995"<-0
WTO_mem$"1995"[WTO_mem$Year<1996]<-1
WTO_mem$"1996"<-0
WTO_mem$"1996"[WTO_mem$Year<1997]<-1
WTO_mem$"1997"<-0
WTO_mem$"1997"[WTO_mem$Year<1998]<-1
WTO_mem$"1998"<-0
WTO_mem$"1998"[WTO_mem$Year<1999]<-1
WTO_mem$"1999"<-0
WTO_mem$"1999"[WTO_mem$Year<2000]<-1
WTO_mem$"2000"<-0
WTO_mem$"2000"[WTO_mem$Year<2001]<-1
WTO_mem$"2001"<-0
WTO_mem$"2001"[WTO_mem$Year<2002]<-1
WTO_mem$"2002"<-0
WTO_mem$"2002"[WTO_mem$Year<2003]<-1
WTO_mem$"2003"<-0
WTO_mem$"2003"[WTO_mem$Year<2004]<-1
WTO_mem$"2004"<-0
WTO_mem$"2004"[WTO_mem$Year<2005]<-1
WTO_mem$"2005"<-0
WTO_mem$"2005"[WTO_mem$Year<2006]<-1
WTO_mem$"2006"<-0
WTO_mem$"2006"[WTO_mem$Year<2007]<-1
WTO_mem$"2007"<-0
WTO_mem$"2007"[WTO_mem$Year<2008]<-1
WTO_mem$"2008"<-0
WTO_mem$"2008"[WTO_mem$Year<2009]<-1
WTO_mem$"2009"<-0
WTO_mem$"2009"[WTO_mem$Year<2010]<-1
WTO_mem$"2010"<-0
WTO_mem$"2010"[WTO_mem$Year<2011]<-1
WTO_mem$"2011"<-0
WTO_mem$"2011"[WTO_mem$Year<2012]<-1
WTO_mem$"2012"<-0
WTO_mem$"2012"[WTO_mem$Year<2013]<-1
WTO_mem$"2013"<-0
WTO_mem$"2013"[WTO_mem$Year<2014]<-1
WTO_mem$"2014"<-0
WTO_mem$"2014"[WTO_mem$Year<2015]<-1
WTO_mem$"2015"<-0
WTO_mem$"2015"[WTO_mem$Year<2016]<-1

WTO_tnam<-WTO_mem[,c("ISO", "Year", "1990":"2015")]




#Trade_dyads_abc_trades<-merge(one_Trade_dyads_ab_trades, WTO_mem, by=c("importer1"), all = FALSE)
#Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year==2006]<-1
#Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year>1]<-0
######Trade
Trade_dyads<-read.csv("~/Desktop/Network Data Construction/COW_Trade_3.0/dyadic_trade_3.0.csv",stringsAsFactors=F)
#Trade_dyads_i<-Trade_dyads[Trade_dyads$year==2006,]
#head(Trade_dyads)
Trade_dyads_a<-Trade_dyads[,c("importer1", "importer2", "flow1", "year")]
Trade_dyads_b<-Trade_dyads[,c("importer2", "importer1", "flow2", "year")]
names(Trade_dyads_a) <- c("ISO", "ISO_source", "flow1", "Year")
names(Trade_dyads_b) <- c("ISO", "ISO_source", "flow1", "Year")
Trade_dyads_ab<-rbind(Trade_dyads_a, Trade_dyads_b)

###fix
Trade_dyads_ab$ISO<-as.factor(countrycode(Trade_dyads_ab$ISO, "country.name",  "cowc"))
Trade_dyads_ab$ISO_source<-as.factor(countrycode(Trade_dyads_ab$ISO_source, "country.name",  "cowc"))
Trade_dyads_abc <- merge(international_econ_data, Trade_dyads_ab, by= c("ISO", "Year"))

Trade_dyads_abc$GDP<-as.numeric(Trade_dyads_abc$GDP.per.capita..constant.2010.US....NY.GDP.PCAP.KD.)*as.numeric(Trade_dyads_abc$Population..total..SP.POP.TOTL.)
Trade_dyads_abc$imp_gdp<-(1000000*Trade_dyads_abc$flow1)/Trade_dyads_abc$GDP

trade_edgy_listy<-Trade_dyads_abc[,c("Year", "ISO", "ISO_source", "imp_gdp")]

######Industry Likeness
###Come back to this
#Indus_Corr<-read.csv("~/Desktop/Network Data Construction/Data_Extract_From_World_Development_Indicators-2/Indus_comp.csv",stringsAsFactors=F)
#High_tech_manu<-read.csv("~/Desktop/Network Data Construction/Competitive_Manu/high_tech_comp.csv",stringsAsFactors=F)
#head(Indus_Corr)
#####







######form matrices

links <- as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1994",])
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- ordering
rownames(adjmat) <- ordering
adjmat[links[,2:3]] <- as.numeric(links[,4])

links <- as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1995",])
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat1 <- matrix(0, length(labels), length(labels))
colnames(adjmat1) <- ordering
rownames(adjmat1) <- ordering
adjmat1[links[,2:3]] <- as.numeric(links[,4])

links <- as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1996",])
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat2 <- matrix(0, length(labels), length(labels))
colnames(adjmat2) <- ordering
rownames(adjmat2) <- ordering
adjmat2[links[,2:3]] <- as.numeric(links[,4])
#######
hga <- list(adjmat, adjmat1, adjmat2) # create list from oldest to newest. This will be the outcome object for TERGM analysis

adjmat11111<-as.data.frame(as.numeric(as.character(adjmat)))

library(xergm)
library(tnam)
model1 <- tnam(adjmat11111~1)
length(soc_tax_tnam_w_test$`2003`)

soc_tax_tnam_w


links <- as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1996",])
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat2 <- matrix(0, length(labels), length(labels))
colnames(adjmat2) <- ordering
rownames(adjmat2) <- ordering
adjmat2[links[,2:3]] <- as.numeric(links[,4])

gdp_tnam_matricized<-data.matrix(gdp_tnam)
head(gdp_tnam_matricized)
#####trial run
library(xergm)
library(tnam)
model1 <- tnam(soc_tax_tnam_w_test ~ 1)
length(soc_tax_tnam_w_test$`2003`)


?tnam

any
covariate(WTO_mem, coefname = "1995"))


+
  covariate(gdp_data_tnam, coefname = "GDP_cap") +
  netlag(soc_tax_tnam_w, hga))

+
  re.node = TRUE, time.linear = TRUE)




###constrain data
row_naming<-soc_tax_tnam_w$ISO
soc_tax_tnam_w_pretest<-soc_tax_tnam_w[,c("1994", "1995", "1996")]
rownames(soc_tax_tnam_w_pretest)<-row_naming
soc_tax_tnam_w_pretest

names(soc_tax_tnam_w_pretest) <- c("t1", "t2", "t3")
soc_tax_tnam_w_pretest



soc_tax_tnam_w_pretest<-data.matrix(soc_tax_tnam_w_pretest)

soc_tax_tnam_w_pretest<-as.data.frame(soc_tax_tnam_w_pretest)
writers_numeric_matrix <- as.matrix(sapply(writers_df, as.numeric))


soc_tax_tnam_w_pretest<-as.data.frame(soc_tax_tnam_w_pretest)
soc_tax_tnam_w_pretest



row_naming<-soc_tax_tnam_w$ISO
soc_tax_tnam_w_pretest<-soc_tax_tnam_w[,c("1994", "1995", "1996")]
rownames(soc_tax_tnam_w_pretest)<-row_naming
soc_tax_tnam_w_pretest<-data.matrix(soc_tax_tnam_w_pretest)
soc_tax_tnam_w_pretest<-as.data.frame(soc_tax_tnam_w_pretest)
writers_numeric_matrix <- as.matrix(sapply(writers_df, as.numeric))
soc_tax_tnam_w_pretest<-as.data.frame(soc_tax_tnam_w_pretest)
names(soc_tax_tnam_w_pretest) <- c("t1", "t2", "t3")
class(soc_tax_tnam_w_pretest)
soc_tax_tnam_w_test
soc_tax_tnam_w_test<-as.data.frame(soc_tax_tnam_w[,c("1994", "1995", "1996")])
rownames(soc_tax_tnam_w_test) <- row_naming
#soc_tax_tnam_w_test<-na.omit(soc_tax_tnam_w_test)
soc_tax_tnam_w_test
class(soc_tax_tnam_w_test$`1994`)
#soc_tax_tnam_w_test$`1994`<-as.numeric(soc_tax_tnam_w_test$`1994`)
#soc_tax_tnam_w_test$`1995`<-as.numeric(soc_tax_tnam_w_test$`1995`)
#soc_tax_tnam_w_test$`1996`<-as.numeric(soc_tax_tnam_w_test$`1996`)
#####
trialcase<-as.data.frame(gdp_tnam)

gdp_tnam

model1 <- tnam(trialcase ~ 1)
?tnam

any
    covariate(WTO_mem, coefname = "1995"))


    +
    covariate(gdp_data_tnam, coefname = "GDP_cap") +
    netlag(soc_tax_tnam_w, hga))
  
  +
  re.node = TRUE, time.linear = TRUE)
?netlag
screenreg(model1, single.row = TRUE)



##############################
####SSHRC Replication Code####
#
#Key findings line 290
#
#
####The Library of Babel######
##############################
library(foreign)
library(plyr)
library(car)
library(psych)
library(stargazer)
library(xtable)
library(stats)
library(multilevel)
library(texreg)
library(ggplot2)
library(nlme)
#############################
#########DATASETS############
#############################

#############################
###########INFORMAL##########
#############################
char_data <- read.csv("~/GitHub/SSHRC_Replication_File/case_informal.csv", header = TRUE, stringsAsFactors = F)
num_data <- data.frame(data.matrix(char_data))
numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
final_data <- data.frame(num_data[,numeric_columns], char_data[,!numeric_columns])
final_data<-rename(final_data, c(char_data....numeric_columns.="Country"))
isn_long<-reshape(final_data,
                  varying = c("X1999","X2000","X2001", "X2002", "X2003", "X2004","X2005","X2006", "X2007"),
                  times = c("X1999","X2000","X2001", "X2002", "X2003", "X2004","X2005","X2006", "X2007"),
                  timevar = "year", direction="long", idvar="Country", v.names = "Informal")
isn_long$Country<-revalue(isn_long$Country, c("Bosnia & Herzegovina"="Bosnia and Herzegovina"))
isn_long<-rename(isn_long, c(year="Year"))
#prep Informal Sector for merge
isn_long$Year <- gsub("X", "", paste(isn_long$Year))

#########################
#####Ensure Merge########
#########################
###as revealed below, problem countries...see if this corrects

#added data set for general econ data
international_economic_data<-read.csv("/Users/Kevin/Downloads/international_economy.csv", header = TRUE)
international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.<-as.numeric(gsub(",", "", international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.))
international_economic_data$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.<-as.numeric(gsub(",", "", international_economic_data$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.))
international_economic_data$GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.<-as.numeric(gsub(",", "", international_economic_data$GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.))
international_economic_data$GINI.index..World.Bank.estimate...SI.POV.GINI.<-as.numeric(gsub(",", "", international_economic_data$GINI.index..World.Bank.estimate...SI.POV.GINI.))
international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.<-as.numeric(gsub(",", "", international_economic_data$GDP..constant.2005.US....NY.GDP.MKTP.KD.))
international_economic_data$Population..total..SP.POP.TOTL.<-as.numeric(gsub(",", "", international_economic_data$Population..total..SP.POP.TOTL.))

#####
####summary(as.numeric(international_economic_data$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.))
#international_economic_data_test<-na.omit(international_economic_data[ ,c("Country", "Year", "GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.",
# "GDP..constant.2005.US....NY.GDP.MKTP.KD.", "GDP.per.capita.growth..annual.....NY.GDP.PCAP.KD.ZG.")])

#View(international_economic_data_test)
names(international_economic_data)[names(international_economic_data)=="Time"] <- "Year"
names(international_economic_data)[names(international_economic_data)=="Country.Name"] <- "Country"
#now merge general econ into WVS
WVS_Econ<-merge(WVS_Work_Leisure_Scores, international_economic_data, by = c("Country"), all = TRUE)
#unique(WVS_Informal_Econ$Country)
#test_set<-na.omit(WVS_Econ[ ,c("Country", "Work_Index", "prop_high", "ISO", "Year")])
##Down to Taiwan only missing
#unique(international_economic_data$Country)
#unique(WVS_Work_Leisure_Scores$Country)
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

WVS_Econ_Informal<-merge(WVS_Econ, isn_long, by = c("Country", "Year"), all = TRUE)
#test_set<-na.omit(WVS_Econ_Informal[ ,c("Country", "Work_Index", "prop_high", "ISO", "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.")])
#note, drops Montenegro, Puerto Rico and Serbia from WVS...these do not exist in Informal
#unique(isn_long$Country)
#unique(WVS_Work_Leisure_Scores$Country)
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

##add polity score
polity<-read.csv("~/GitHub/SSHRC_Replication_File/p4v2014.csv")
#head(polity)
polity<-rename(polity, c(year="Year", country="Country"))
polity$Country<-as.factor(polity$Country)
polity$Country<-revalue(polity$Country, c("Bosnia"="Bosnia and Herzegovina", "Kyrgyzstan"="Kyrgyz Republic", "Korea South"="Korea, Rep.",
                                          "Iran"="Iran, Islamic Rep.", "Russia"="Russian Federation", "Macedonia"="Macedonia, FYR", "Egypt"="Egypt, Arab Rep.",
                                          "Venezuela"="Venezuela, RB"))
####NAME Doctor_Who since all of time and space... well...
Doctor_Who<-merge(WVS_Econ_Informal_Tax, polity, by=c("Country", "Year"), all = TRUE)
#okay, appears no option but to drop Puerto Rico due to abscence from polity
#test_set<-na.omit(Doctor_Who[ ,c("Country", "Work_Index", "prop_high", "GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.", "Income", "polity2")])
#results1 = setdiff(WVS_Work_Leisure_Scores$Country, test_set$Country)
#results1

#######In sum, losing two cases in the WVS, Puerto Rico (no polity, but tax)
###and Taiwan (polity, no tax). Only
#possible good side, both are pretty middling in terms of work ethic scores

################################
####### The #### Dataverse #####
#########Is now merged #########
################################
########Blimey##################

####################################
#####Create Suite of Variables######
####################################
Doctor_Who$Indiv_inc_prof<-as.numeric(sub("%", "", Doctor_Who$Non.resource.component.of.taxes.on.income..profits..and.capital.gains))
Doctor_Who$Social.contributions<-as.numeric(sub("%", "", Doctor_Who$Social.contributions))
Doctor_Who$Social.contributions....of.revenue...GC.REV.SOCL.ZS.<-as.numeric(sub("%", "", Doctor_Who$Social.contributions....of.revenue...GC.REV.SOCL.ZS.))


Doctor_Who$Income_Pro<-as.numeric(sub("%", "", Doctor_Who$Income))
Doctor_Who$Sales<-as.numeric(sub("%", "", Doctor_Who$Taxes.on.goods.and.services..of.which.Taxes.on.Sales))
Doctor_Who$Indiv_Sales<-Doctor_Who$Income_Pro/Doctor_Who$Sales
Doctor_Who$Indiv_Sales[Doctor_Who$Indiv_Sales=="Inf"]<-NA
#summary(Doctor_Who$Indiv_Sales)

Doctor_Who$GDP_cap<-as.numeric(Doctor_Who$GDP.per.capita..constant.2005.US....NY.GDP.PCAP.KD.)
Doctor_Who$GINI<-as.numeric(Doctor_Who$GINI.index..World.Bank.estimate...SI.POV.GINI.)
Doctor_Who$Growth<-as.numeric(Doctor_Who$GDP.growth..annual.....NY.GDP.MKTP.KD.ZG.)
Doctor_Who$UE<-as.numeric(Doctor_Who$Unemployment..total....of.total.labor.force...modeled.ILO.estimate...SL.UEM.TOTL.ZS.)


Doctor_Who$agri<-as.numeric(gsub(",", "", Doctor_Who$Employment.in.agriculture....of.total.employment...SL.AGR.EMPL.ZS.))

Doctor_Who$GDP<-as.numeric(Doctor_Who$GDP..constant.2005.US....NY.GDP.MKTP.KD.)
Doctor_Who$Year<-as.numeric(Doctor_Who$Year)
Doctor_Who$Informal<-as.numeric(Doctor_Who$Informal)


