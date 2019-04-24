setwd("Q:/Data/PIU/Export_Diversification/datacleaning")
# Data manipulation
library(randomcoloR)
library(tidyverse)
library(reshape)
library(dplyr)
library(ggpubr)
library(RColorBrewer)

lidcss <- read.csv("Q:/Data/PIU/Export_Diversification/comtrade/bycountry/lidcdata.csv",check.names = "false")
colnames(lidcss)[4:13]<-c("g_food","g_bev","g_crd","g_min","g_ani","g_chem","g_mfc","g_mchn","g_misl","g_others")
sapply(lidcss,class)
lidcss[4:13]<-sapply(lidcss[4:13],as.character)
lidcss[4:13]<-sapply(lidcss[4:13],as.numeric)

#change goods magnitude to US$ million 

lidcss[4:13] <- lidcss[4:13]/ 1000000
lidctable <- as_tibble(lidcss[-1])

# remove all NA rows and generate max of each row
lidctable <- lidctable[apply(lidctable[-c(1:2,13)],1,function(x)any(!is.na(x))),]

# lidctable$max2 <- apply(lidctable[,c(3:12,14:25)],1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [1])
# lidctable$max2 <- apply(lidctable[,c(3:12,14:25)],1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [2])
# lidctable$max3 <- apply(lidctable[,c(3:12,14:25)],1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [3])
# lidctable$max4 <- apply(lidctable[,c(3:12,14:25)],1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [4])
# lidctable$max5 <- apply(lidctable[,c(3:12,14:25)],1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [5])

#turn above 5 into a function
maxfunc <- function(x,y) {
  apply(x, 1, function(i) i = sort(unique(i), decreasing = TRUE, na.last = TRUE) [y])
}

# forloop to generate top n values of each row
for (i in 1:6) {
  lidctable[,i+25] <- maxfunc(lidctable[-c(1:2,13)],i) # create the value first in the loop
  colnames(lidctable)[i+25] <- paste("max",i,sep="") # rename the variable later
}

# generate goods total and clean up fake zeros
lidctable <- cbind(lidctable, g_total = rowSums(lidctable[3:12],na.rm = TRUE))
lidctable$g_total[lidctable$g_total == 0] <- NA
lidctable <- cbind(lidctable, s_total = rowSums(lidctable[14:25],na.rm = TRUE))
lidctable$s_total[lidctable$s_total == 0] <- NA

# calculate diversification index

# sum of squares

for (i in 2:6){
      lidctable[,i+32] <- 1/((rowSums(lidctable[26:(25+i)],na.rm = TRUE))^2) 
      colnames(lidctable)[i+32] <- paste("idxd",i,sep="_") # rename the variable later
}

for (i in 1:6) {
  lidctable[,i+38] <-(lidctable[,(i+25)])^2
  colnames(lidctable)[i+38] <- paste("idxcp",i,sep="_") # rename the variable later
}

# Calculate div index base on the top n industries export sum
for (i in 1:5) {
  lidctable[,(i+44)] <- (rowSums(lidctable[39:(39+i)],na.rm = TRUE))*(lidctable[,(i+33)])
  colnames(lidctable)[i+44]<- paste("divindex",i,sep = "")
}

lidcdiv <- lidctable[-c(13)]

for (i in 1:22) {
  lidcdiv [,(i+48)] <- lidcdiv[,(i+2)]^2
  colnames(lidcdiv)[i+48]<- paste("gs_square",(i+2),sep = "")
}

#Calculate div index based on export total

lidcdiv$exptotalsquare <- rowSums(lidcdiv[31:32],na.rm = TRUE)^2
lidcdiv$divindexall <- (rowSums(lidcdiv[49:70], na.rm = TRUE))/lidcdiv$exptotalsquare

for (i in 2:6) {
  lidcdiv[,(i+71)] <- (rowSums(lidcdiv[38:(i+37)], na.rm = TRUE))/lidcdiv$exptotalsquare
  colnames(lidcdiv)[i+71]<- paste("divindexall",i,sep = "")
}

#Calculate div index based on goods/services total

lidcdiv$idx_g <- (rowSums(lidcdiv[49:58],na.rm= TRUE))/(lidcdiv$g_total)^2
lidcdiv$idx_s <- (rowSums(lidcdiv[59:70],na.rm= TRUE))/(lidcdiv$s_total)^2



divindex <- cbind(lidctable[13],lidcdiv[c(1:2,31,44:48,72:79)])
lidcdiv <- cbind(lidctable[13],lidcdiv)

#write.csv(divindex,"diversification_index.csv")
#write.csv(lidcdiv,"divindexcal.csv")


###############################################################
###############################################################
###############################################################

#read WB infra data
wb_data <- read.csv("Q:/Data/PIU/Export_Diversification/datacleaning/wbinfrast1.csv",check.names = FALSE)
library(reshape)
wb_data1<- melt(wb_data,id=c("CountryCode","IndicatorCode")) #from the reshape package
colnames(wb_data1)[3] <- "year"
wb_data2<- reshape(wb_data1,idvar=c("CountryCode","year"), timevar= "IndicatorCode", direction = "wide")
#write.csv(wb_data2, file = "wb_data_all.csv")

#read EMDAT data
emdat <- read.csv("//DATA2/APD/Data/PIU/Export_Diversification/datacleaning/EMDAT-data2018.csv",check.names = FALSE)
emdat <- emdat[,c(1:2,11,15:16)]
colnames(emdat)[c(1:2,5)] <- c("Year","country","totaldamage")
emdat1 <- aggregate(.~Year+country,data = emdat,sum, na.rm= TRUE)

expdivdb <- merge(lidcdiv[-c(34:44,50:71)],emdat1, by = c("country","Year"), all = TRUE)

colnames(wb_data2)[1:2] <- c("country","Year")
expdivdb1 <- merge(expdivdb, wb_data2, by = c("country","Year"), all = TRUE)
expdivdb2 <- expdivdb1[apply(expdivdb1[c(3:24)],1,function(x)any(!is.na(x))),]

#read credit data
cred <- read.csv("//DATA2/APD/Data/PIU/Export_Diversification/datacleaning/credit data.csv",check.names = FALSE)
cred1 <- cred[c(1,3,5,13,24,29,34,37,38,40,41,50)]
colnames(cred1)[1] <- "country"
expdivdb3 <- merge(expdivdb2, cred1, by = c("country","Year"), all = TRUE)
expdivdb4 <- expdivdb3[apply(expdivdb3[c(3:24)],1,function(x)any(!is.na(x))),]


#read ICT data
ict<-read.csv("//DATA2/APD/Data/PIU/Export_Diversification/datacleaning/ict.csv")
ict1<- cbind(ict[2:3],ict$ITNETUSERZS,ict$ITNETSECR,ict$ITNETSECRP6,ict$FI_OverallIndex_GCI,ict$FI_Corruption_GCI,ict$FI_PublicInstitutions_GCI,ict$FI_FRAGILE2016_SPR,ict$FI_FX1_ARREARS,ict$FI_FX2_ARREARS,ict$FI_FX3_ARREARS,ict$FI_FX4_ARREARS,ict$FI_FX5_ARREARS,ict$FI_FX6_ARREARS,ict$FI_FX7_ARREARS,ict$FI_FX8_ARREARS,ict$FI_FX9_ARREARS,ict$FI_FX10_ARREARS,ict$FI_GDPPC_WDI,ict$FI_GNI_WDI,ict$FI_GNIPC_WDI,ict$FI_FXUSDPA_WEO,ict$FI_FXNEER_INS,ict$FI_FXREER_INS,ict$FI_CPI_INS)
colnames(ict1)[1:2] <- c("country_code","Year")

expdivdb5 <- merge(expdivdb4,ict1,by = c("country_code","Year"),all = TRUE)
expdivdb6 <- expdivdb5[apply(expdivdb5[c(3:25)],1,function(x)any(!is.na(x))),]

#top 5 goods
for (i in 1:5) {
  expdivdb6[,i+98] <- maxfunc(expdivdb6[c(4:13)],i) # create the value first in the loop
  colnames(expdivdb6)[i+98] <- paste("max_g",i,sep="") # rename the variable later
}

#top 5 services
for (i in 1:5) {
  expdivdb6[,i+103] <- maxfunc(expdivdb6[c(14:25)],i) # create the value first in the loop
  colnames(expdivdb6)[i+103] <- paste("max_s",i,sep="") # rename the variable later
}


write.csv(expdivdb6,"expdivdb_final-R.csv")

