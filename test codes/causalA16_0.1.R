######DATA PREPERATION######

######Tax data
international_tax_data<-read.csv("~/Desktop/Network Data Construction/ICTDGRD_June2016_CentralGeneralMerged/Merged-Table 1.csv")
names(international_tax_data)[names(international_tax_data)=="Calendar.year..nearest."] <- "Year"
head(international_tax_data)

#Likeness & Covariates#
######GDP per cap, Population, 
international_econ_data<-read.csv("~/Desktop/Network Data Construction/Data_Extract_From_World_Development_Indicators (5) 2/country_stats.csv")
head(international_econ_data)

######Polity
##add polity score
polity<-read.csv("~/Desktop/Network Data Construction/Polity/p4v2014.csv")
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

#Trade_dyads_abc_trades<-merge(one_Trade_dyads_ab_trades, WTO_mem, by=c("importer1"), all = FALSE)
#Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year==2006]<-1
#Trade_dyads_abc_trades$Year[Trade_dyads_abc_trades$Year>1]<-0



######Trade
Trade_dyads<-read.csv("~/Desktop/Network Data Construction/COW_Trade_3.0/dyadic_trade_3.0.csv",stringsAsFactors=F)
#Trade_dyads_i<-Trade_dyads[Trade_dyads$year==2006,]
#head(Trade_dyads)
Trade_dyads_a<-Trade_dyads[,c("importer1", "importer2", "flow1", "year")]
Trade_dyads_b<-Trade_dyads[,c("importer2", "importer1", "flow2", "year")]
names(Trade_dyads_b) <- c("importer1", "importer2", "flow1", "year")
Trade_dyads_ab<-rbind(Trade_dyads_a, Trade_dyads_b)
Trade_dyads_ab$importer1<-as.factor(countrycode(Trade_dyads_ab$importer1, "country.name",  "cowc"))
Trade_dyads_ab$importer2<-as.factor(countrycode(Trade_dyads_ab$importer2, "country.name",  "cowc"))
head(Trade_dyads_ab)


######Industry Likeness
Indus_Corr<-read.csv("~/Desktop/Network Data Construction/Data_Extract_From_World_Development_Indicators-2/Indus_comp.csv",stringsAsFactors=F)
High_tech_manu<-read.csv("~/Desktop/Network Data Construction/Competitive_Manu/high_tech_comp.csv",stringsAsFactors=F)
head(High_tech_manu)



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


