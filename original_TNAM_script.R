######DATA PREPERATION######
library(foreign)
library(reshape2)
######Tax data
international_tax_data<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
names(international_tax_data)[names(international_tax_data)=="Calendar.year..nearest."] <- "Year"
#Likeness & Covariates#
######GDP per cap, Population
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
#####WTO Member, GATT
###also, now get in some world trade org data
WTO_mem<-read.csv("~/Desktop/Network Data Construction/wto_mem.csv",stringsAsFactors=F)
library(stringr)
WTO_mem$Year<-as.numeric(str_sub(WTO_mem$WTO.Accession.Date, -4, -1))
WTO_mem$Year[is.na(WTO_mem$Year)]<-9999
names(WTO_mem)[names(WTO_mem)=="Country.Name"] <- "importer1a"
library(countrycode)
WTO_mem$importer1<-as.factor(countrycode(WTO_mem$importer1a, "country.name",  "cowc"))
head(WTO_mem)
#now establish data in form needed
######DVs..... Tax data
international_tax_data$Social_Security<-as.numeric(sub("%", "", international_tax_data$Social.contributions))
#international_tax_data$Corp<-as.numeric(sub("%", "", international_tax_data$Corp))
international_tax_data$Corporate<-as.numeric(sub("%", "", international_tax_data$Corporations.and.other.enterprises))
#########pull out only needed data#########
soc_tax_data_tnam<-na.omit(international_tax_data[,c("ISO", 
                                                     "Year",
                                                     "Social_Security")])
corp_tax_data_tnam<-international_tax_data[,c("ISO", 
                                              "Year",
                                              "Corporate")]













soc_tax_data_tnam$ISO <- as.character(soc_tax_data_tnam$ISO)
corp_tax_data_tnam$ISO <- as.character(corp_tax_data_tnam$ISO)
widedata <- reshape(soc_tax_data_tnam, timevar = "Year", v.names = c("Social_Security"),
                    idvar = c("ISO"), direction = "wide")
widedata_c <- reshape(corp_tax_data_tnam, timevar = "Year", v.names = c("Corporate"),
                      idvar = c("ISO"), direction = "wide")

widedata$Social_Security.1993[is.na(widedata$Social_Security.1993)]<-0
widedata$Social_Security.1994[is.na(widedata$Social_Security.1994)]<-0
widedata$Social_Security.1995[is.na(widedata$Social_Security.1995)]<-0
widedata$Social_Security.1996[is.na(widedata$Social_Security.1996)]<-0
widedata$Social_Security.1997[is.na(widedata$Social_Security.1997)]<-0

soc93 <- widedata$Social_Security.1993
names(soc93) <- widedata$ISO
soc94 <- widedata$Social_Security.1994
names(soc94) <- widedata$ISO
soc95 <- widedata$Social_Security.1995
names(soc95) <- widedata$ISO
soc96 <- widedata$Social_Security.1996
names(soc96) <- widedata$ISO
soc97 <- widedata$Social_Security.1997
names(soc97) <- widedata$ISO

labor <- list(t1 = soc93, t2 = soc94, t3 = soc95, t4 = soc96, t5 = soc97)

cor93 <- widedata_c$Corporate.1993
names(cor93) <- widedata_c$ISO
cor94 <- widedata_c$Corporate.1994
names(cor94) <- widedata_c$ISO
cor95 <- widedata_c$Corporate.1995
names(cor95) <- widedata_c$ISO
cor96 <- widedata_c$Corporate.1996
names(cor96) <- widedata_c$ISO
cor97 <- widedata_c$Corporate.1997
names(cor97) <- widedata_c$ISO

capital <- list(t1 = cor93, t2 = cor94, t3 = cor95, t4 = cor96, t5 = cor97)

######
#####
#####
#####

######Onto IVs as covariates#######
international_econ_data$GDP_cap<-as.numeric(international_econ_data$GDP.per.capita..constant.2010.US....NY.GDP.PCAP.KD.)
international_econ_data$Pop<-as.numeric(international_econ_data$Population..total..SP.POP.TOTL.)
international_econ_data$GINI<-as.numeric(international_econ_data$GINI.index..World.Bank.estimate...SI.POV.GINI.)
international_econ_data$ISO<-international_econ_data$Country.Code
international_econ_data$Year<-international_econ_data$Time

gdp_data_tnam<-international_econ_data[,c("ISO", 
                                          "Year",
                                          "GDP_cap")]
#gdp_data_tnam<-merge(gdp_data_tnam, 
#subset(merge(dat1, dat2, by = c("ID", "block", "plot"), all.y = TRUE), is.na(SPID.x) == TRUE)
gdp_data_tnam$ISO <- as.character(gdp_data_tnam$ISO)
widedata <- reshape(gdp_data_tnam, timevar = "Year", v.names = c("GDP_cap"),
                    idvar = c("ISO"), direction = "wide")
gdp93 <- log(widedata$GDP_cap.1993)
names(gdp93) <- widedata$ISO
gdp94 <- log(widedata$GDP_cap.1994)
names(gdp94) <- widedata$ISO
gdp95 <- log(widedata$GDP_cap.1995)
names(gdp95) <- widedata$ISO
gdp96 <- log(widedata$GDP_cap.1996)
names(gdp96) <- widedata$ISO
gdp97 <- log(widedata$GDP_cap.1997)
names(gdp97) <- widedata$ISO

gdp_at_threse <- list(t1 = gdp93, t2 = gdp94, t3 = gdp95, t4 = gdp96, t5 = gdp97)




gdp_data_tnam_sim<-merge(gdp_data_tnam, soc_tax_data_tnam, by = c("ISO", "Year"), all.y = TRUE)
gdp_data_tnam_sim<-gdp_data_tnam_sim[,c("ISO", 
                                        "Year",
                                        "GDP_cap")]
gdp_data_tnam_sim$GDP_cap[is.na(gdp_data_tnam_sim$GDP_cap)]<-100
widedata <- reshape(gdp_data_tnam_sim, timevar = "Year", v.names = c("GDP_cap"),
                    idvar = c("ISO"), direction = "wide")
widedata$GDP_cap.1993[is.na(widedata$GDP_cap.1993)]<-100
widedata$GDP_cap.1994[is.na(widedata$GDP_cap.1994)]<-100
widedata$GDP_cap.1995[is.na(widedata$GDP_cap.1995)]<-100
widedata$GDP_cap.1996[is.na(widedata$GDP_cap.1996)]<-100
widedata$GDP_cap.1997[is.na(widedata$GDP_cap.1997)]<-100
widedata$GDP_cap.1993[widedata$GDP_cap.1993<100]<-100
widedata$GDP_cap.1994[widedata$GDP_cap.1994<100]<-100
widedata$GDP_cap.1995[widedata$GDP_cap.1995<100]<-100
widedata$GDP_cap.1996[widedata$GDP_cap.1996<100]<-100
widedata$GDP_cap.1997[widedata$GDP_cap.1997<100]<-100




gdp93_bin <- as.factor(as.numeric(widedata$GDP_cap.1993>=7000))
names(gdp93_bin) <- widedata$ISO
gdp94_bin <- as.factor(as.numeric(widedata$GDP_cap.1994>=7000))
names(gdp94_bin) <- widedata$ISO
gdp95_bin <- as.factor(as.numeric(widedata$GDP_cap.1995>=7000))
names(gdp95_bin) <- widedata$ISO
gdp96_bin <- as.factor(as.numeric(widedata$GDP_cap.1996>=7000))
names(gdp96_bin) <- widedata$ISO
gdp97_bin <- as.factor(as.numeric(widedata$GDP_cap.1997>=7000))
names(gdp97_bin) <- widedata$ISO


widedata$GDP_cap.1993
gdp93_bin <- log(widedata$GDP_cap.1993)
names(gdp93_bin) <- widedata$ISO
gdp94_bin <- log(widedata$GDP_cap.1994)
names(gdp94_bin) <- widedata$ISO
gdp95_bin <- log(widedata$GDP_cap.1995)
names(gdp95_bin) <- widedata$ISO
gdp96_bin <- log(widedata$GDP_cap.1996)
names(gdp96_bin) <- widedata$ISO
gdp97_bin <- log(widedata$GDP_cap.1997)
names(gdp97_bin) <- widedata$ISO

gdp_at_threse_sim <- list(t1 = gdp93_bin, t2 = gdp94_bin, 
                          t3 = gdp95_bin, t4 = gdp96_bin, 
                          t5 = gdp97_bin)

gdp_at_threse_sim
log(100)
#pop_data_tnam<-international_econ_data[,c("ISO", 
#                                          "Year",
#                                          "Pop")]

#gini_data_tnam<-international_econ_data[,c("ISO", 
#                                           "Year",
#                                           "GINI")]



#gdp_tnam <- dcast(gdp_data_tnam, ISO ~ Year, value.var="GDP_cap", fun.aggregate = median)
#pop_tnam <- dcast(pop_data_tnam, ISO ~ Year, value.var="Pop", fun.aggregate = median)
#gini_tnam <- dcast(gini_data_tnam, ISO ~ Year, value.var="GINI", fun.aggregate = median)


####now from polity
polity$ISO<-polity$scode
polity_data_tnam<-polity[,c("ISO", "Year","polity2")]
polity_data_tnam$ISO<-as.character(polity_data_tnam$ISO)
#polity_tnam <- dcast(polity_data_tnam, ISO ~ Year, value.var="polity2")
widedata <- reshape(polity_data_tnam, timevar = "Year", v.names = c("polity2"),
                    idvar = c("ISO"), direction = "wide")
pol93 <- widedata$polity2.1993
names(pol93) <- widedata$ISO
pol94 <- widedata$polity2.1994
names(pol94) <- widedata$ISO
pol95 <- widedata$polity2.1995
names(pol95) <- widedata$ISO
pol96 <- widedata$polity2.1996
names(pol96) <- widedata$ISO
pol97 <- widedata$polity2.1997
names(pol97) <- widedata$ISO

poli <- list(t1 = pol93, t2 = pol94, t3 = pol95, t4 = pol96, t5 = pol97)

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
WTO_tnam$ISO<-as.character(WTO_tnam$ISO)
WTO_tnam$Year<-as.character(WTO_tnam$Year)

#widedata_c <- reshape(corp_tax_data_tnam, timevar = "Year", v.names = c("Corporate"),
#                      idvar = c("ISO"), direction = "wide")

wto93 <- WTO_tnam$"1993"
names(wto93) <- WTO_tnam$ISO
wto94 <- WTO_tnam$"1994"
names(wto94) <- WTO_tnam$ISO
wto95 <- WTO_tnam$"1995"
names(wto95) <- WTO_tnam$ISO
wto96 <- WTO_tnam$"1996"
names(wto96) <- WTO_tnam$ISO
wto97 <- WTO_tnam$"1997"
names(wto97) <- WTO_tnam$ISO
WTO_members <- list(t1 = wto93, t2 = wto94, t3 = wto95, t4 = wto96, t5 = wto97)

#summary(tnam(labor ~ covariate(WTO_members, lag = 1), re.node = T ))

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


links <- na.omit(as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1993",]))
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- ordering
rownames(adjmat) <- ordering
adjmat[links[,2:3]] <- as.numeric(links[,4])


links <- na.omit(as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1994",]))
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat1 <- matrix(0, length(labels), length(labels))
colnames(adjmat1) <- ordering
rownames(adjmat1) <- ordering
adjmat1[links[,2:3]] <- as.numeric(links[,4])


links <- na.omit(as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1995",]))
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat2 <- matrix(0, length(labels), length(labels))
colnames(adjmat2) <- ordering
rownames(adjmat2) <- ordering
adjmat2[links[,2:3]] <- as.numeric(links[,4])


links <- na.omit(as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1996",]))
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat3 <- matrix(0, length(labels), length(labels))
colnames(adjmat3) <- ordering
rownames(adjmat3) <- ordering
adjmat3[links[,2:3]] <- as.numeric(links[,4])


links <- na.omit(as.matrix(trade_edgy_listy[trade_edgy_listy$Year=="1997",]))
labels <- unique(c(links[,2], links[,3]))
ordering <- sort(labels) # put into alphabetical order
adjmat4 <- matrix(0, length(labels), length(labels))
colnames(adjmat4) <- ordering
rownames(adjmat4) <- ordering
adjmat4[links[,2:3]] <- as.numeric(links[,4])


#######
hga <- list(t1 = adjmat, t2 = adjmat1, 
            t3 = adjmat2, t4 = adjmat3, 
            t5 = adjmat4) # create list from oldest to newest. This will be the outcome object for TERGM analysis



######make WTO as network
WTO_a<-WTO_tnam[,c("ISO", "1993")]
colnames(WTO_a)<-c("ISO", "WTO_mem")
WTO_trade_a<-merge(trade_edgy_listy, WTO_a, by = "ISO")
WTO_trade_a<-WTO_trade_a[,c("ISO", "ISO_source", "WTO_mem", "Year")]
WTO_trade_a$WTO_mem_new<-0
WTO_trade_a$WTO_mem_new<-WTO_trade_a$WTO_mem
WTO_trade_a$WTO_mem_new[WTO_trade_a$WTO_mem_new=="NA"]<-0
links <- na.omit(as.matrix(WTO_trade_a[WTO_trade_a$Year=="1993",]))

labels <- unique(c(links[,1], links[,2]))
ordering <- sort(labels) # put into alphabetical order
adjmat <- matrix(0, length(labels), length(labels))
colnames(adjmat) <- ordering
rownames(adjmat) <- ordering
adjmat[links[,1:2]] <- as.numeric(links[,5])


WTO_a<-WTO_tnam[,c("ISO", "1994")]
colnames(WTO_a)<-c("ISO", "WTO_mem")
WTO_trade_a<-merge(trade_edgy_listy, WTO_a, by = "ISO")
WTO_trade_a<-WTO_trade_a[,c("ISO", "ISO_source", "WTO_mem", "Year")]
WTO_trade_a$WTO_mem_new<-0
WTO_trade_a$WTO_mem_new<-WTO_trade_a$WTO_mem
WTO_trade_a$WTO_mem_new[WTO_trade_a$WTO_mem_new=="NA"]<-0
links <- na.omit(as.matrix(WTO_trade_a[WTO_trade_a$Year=="1994",]))

labels <- unique(c(links[,1], links[,2]))
ordering <- sort(labels) # put into alphabetical order
adjmat1 <- matrix(0, length(labels), length(labels))
colnames(adjmat1) <- ordering
rownames(adjmat1) <- ordering
adjmat1[links[,1:2]] <- as.numeric(links[,5])


WTO_a<-WTO_tnam[,c("ISO", "1995")]
colnames(WTO_a)<-c("ISO", "WTO_mem")
WTO_trade_a<-merge(trade_edgy_listy, WTO_a, by = "ISO")
WTO_trade_a<-WTO_trade_a[,c("ISO", "ISO_source", "WTO_mem", "Year")]
WTO_trade_a$WTO_mem_new<-0
WTO_trade_a$WTO_mem_new<-WTO_trade_a$WTO_mem
WTO_trade_a$WTO_mem_new[WTO_trade_a$WTO_mem_new=="NA"]<-0
links <- na.omit(as.matrix(WTO_trade_a[WTO_trade_a$Year=="1995",]))

labels <- unique(c(links[,1], links[,2]))
ordering <- sort(labels) # put into alphabetical order
adjmat2 <- matrix(0, length(labels), length(labels))
colnames(adjmat2) <- ordering
rownames(adjmat2) <- ordering
adjmat2[links[,1:2]] <- as.numeric(links[,5])
adjmat2


WTO_a<-WTO_tnam[,c("ISO", "1996")]
colnames(WTO_a)<-c("ISO", "WTO_mem")
WTO_trade_a<-merge(trade_edgy_listy, WTO_a, by = "ISO")
WTO_trade_a<-WTO_trade_a[,c("ISO", "ISO_source", "WTO_mem", "Year")]
WTO_trade_a$WTO_mem_new<-0
WTO_trade_a$WTO_mem_new<-WTO_trade_a$WTO_mem
WTO_trade_a$WTO_mem_new[WTO_trade_a$WTO_mem_new=="NA"]<-0
links <- na.omit(as.matrix(WTO_trade_a[WTO_trade_a$Year=="1996",]))

labels <- unique(c(links[,1], links[,2]))
ordering <- sort(labels) # put into alphabetical order
adjmat3 <- matrix(0, length(labels), length(labels))
colnames(adjmat3) <- ordering
rownames(adjmat3) <- ordering
adjmat3[links[,1:2]] <- as.numeric(links[,5])


WTO_a<-WTO_tnam[,c("ISO", "1997")]
colnames(WTO_a)<-c("ISO", "WTO_mem")
WTO_trade_a<-merge(trade_edgy_listy, WTO_a, by = "ISO")
WTO_trade_a<-WTO_trade_a[,c("ISO", "ISO_source", "WTO_mem", "Year")]
WTO_trade_a$WTO_mem_new<-0
WTO_trade_a$WTO_mem_new<-WTO_trade_a$WTO_mem
WTO_trade_a$WTO_mem_new[WTO_trade_a$WTO_mem_new=="NA"]<-0
links <- na.omit(as.matrix(WTO_trade_a[WTO_trade_a$Year=="1997",]))

labels <- unique(c(links[,1], links[,2]))
ordering <- sort(labels) # put into alphabetical order
adjmat4 <- matrix(0, length(labels), length(labels))
colnames(adjmat4) <- ordering
rownames(adjmat4) <- ordering
adjmat4[links[,1:2]] <- as.numeric(links[,5])

WTO_member_growth<-list(t1 = adjmat, t2 = adjmat1, t3 = adjmat2, t4 = adjmat3, t5 = adjmat4) # create list from oldest to newest. This will be the outcome object for TERGM analysis





#links<-na.omit(as.matrix(WTO_net))
#labels<-links[,1]

WTO_neta <- matrix(0, length(labels), length(labels))
colnames(WTO_neta) <- labels
rownames(WTO_neta) <- labels
WTO_neta[1:nrow(WTO_net1),]<-links[,6]
WTO_neta<-as.data.frame(WTO_neta)

WTO_netb <- matrix(0, length(labels), length(labels))
colnames(WTO_netb) <- labels
rownames(WTO_netb) <- labels
WTO_netb[1:nrow(WTO_net1),]<-links[,7]
WTO_netb<-as.data.frame(WTO_netb)

WTO_netc <- matrix(0, length(labels), length(labels))
colnames(WTO_netc) <- labels
rownames(WTO_netc) <- labels
WTO_netc[1:nrow(WTO_net1),]<-links[,8]
WTO_netc<-as.data.frame(WTO_netc)

WTO_netd <- matrix(0, length(labels), length(labels))
colnames(WTO_netd) <- labels
rownames(WTO_netd) <- labels
WTO_netd[1:nrow(WTO_net1),]<-links[,9]
WTO_netd<-as.data.frame(WTO_netd)

WTO_nete <- matrix(0, length(labels), length(labels))
colnames(WTO_nete) <- labels
rownames(WTO_nete) <- labels
WTO_nete[1:nrow(WTO_net1),]<-links[,10]
WTO_nete<-as.data.frame(WTO_nete)

WTO_evo <- list(t1 = WTO_neta, t2 = WTO_netb, t3 = WTO_netc, t4 = WTO_netd, t5 = WTO_netd) # create list from oldest to newest. This will be the outcome object for TERGM analysis
dim(adjmat)



library(tnam)

summary(tnam(labor ~ covariate(WTO_members,lag = 1)
             +covariate(gdp_at_threse)
             +covariate(poli)
             +centrality(hga, type = "betweenness"),
             re.node = T, time.linear = T))
###worst case scenario, proceed with something like the following
summary(tnam(labor ~ covariate(WTO_members,lag = 1)
             #+attribsim(labor, poli)
             +netlag(labor, hga, lag = 1)
             #+centrality(hga, type = "betweenness")
             #+clustering(hga)
             #+ weightlag(labor, hga)
             , re.node = T))
?covariate
####works well for capital, but need no attribsim then

summary(tnam(labor ~ covariate(gdp_at_threse, coefname = "gdp_cap")
             + attribsim(labor, gdp_at_threse_sim)
             #covariate(gdp_at_threse, coefname = "gdp_cap")
             +covariate(poli, coefname = "polity")
             +netlag(labor, hga, lag = 3, coefname = "trade")
             +netlag(labor, WTO_member_growth, lag = 3, coefname = "wto")
             +interact(netlag(labor, hga, lag = 3),
                       netlag(labor, WTO_member_growth, lag = 3)),
             re.node = TRUE))

labor_diff<-labor$t5-labor$t1
gdp_diff<-gdp_at_threse$t5-gdp_at_threse$t1
gdp_sim_diff<-gdp_at_threse_sim$t5-gdp_at_threse_sim$t1
hga_diff<-hga$t5-hga$t2
WTO_diff<-WTO_member_growth$t5-WTO_member_growth$t2
labor_diff
summary(tnam(labor_diff ~ covariate(gdp_sim_diff, coefname = "gdp_cap")))
#+ attribsim(labor_diff, gdp_sim_diff)
#covariate(gdp_at_threse, coefname = "gdp_cap")
#+covariate(poli, coefname = "polity")
#+netlag(labor_diff, hga_diff, coefname = "trade")
# +netlag(labor_diff, WTO_diff, coefname = "wto")
# +interact(netlag(labor_diff, hga_diff),
# netlag(labor_diff, WTO_diff))))


#+centrality(hga, type = "betweenness")
#+clustering(hga)
#+ weightlag(labor, hga)
#, re.node = T, 
#time.linear = T))

gdp_at_threse_sim

####
summary(tnam(labor ~ covariate(gdp_at_threse, coefname = "gdp_cap")
             +covariate(poli, coefname = "polity")
             #+centrality(hga, type = "betweenness")
             #+clustering(hga)
             #+ weightlag(labor, hga)
             , re.node = T))

summary(tnam(labor ~ 
               #covariate(WTO_members,lag = 1)
               +attribsim(labor, poli)
             interact(netlag(labor, WTO_evo), netlag(labor, hga))
             #+centrality(hga, type = "betweenness")
             #+clustering(hga)
             #+ weightlag(labor, hga)
             , re.node = T))
?tnam
str(poli)
?tnam
?netlag...match
gdp_at_threse
poli
WTO_members

#m <- lm(labour ~ capital +
#          factor(ISO) + factor(Year), data = panel1)

library(tnam)
library(texreg)
?tnam
successful_trial<-tnam(labor~covariate(labor, lag = 1)+covariate(capital, lag = 1), re.node=T)

screenreg(successful_trial)
library(stargazer)
xtable(successful_trial)






model1 <- tnam(polity ~ covariate(polity, lag = 1, exponent = 1) +
                 covariate(population, coefname = "population") + 
                 covariate(income, coefname = "gdppc") +
                 covariate(socialism, coefname = "socialism"),
               re.node = T)
screenreg(model1, single.row = TRUE)

