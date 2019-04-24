setwd("Q:/Data/PIU/Export_Diversification/comtrade/bycountry")

install.packages('rjson')
library(rjson)
library(magrittr)
library(httr)

library(tidyverse)
library(reshape)

httr::set_config( config( ssl_verifypeer = 0L ) )

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

# get goods data from comtrade in US$ unit
s1<- get.Comtrade(url = "https://comtrade.un.org/api/get?",maxrec = 500000,type = "C",freq = "A",px = "S3",r= "all", ps = c("all"), rg = "2", p = "0", cc = "AG1")
write.csv(s1$data,file = "comtradegoods1.csv")

# subset goods data and reshape
goods <-s1$data[c(2,11,22,32)]
colnames(goods)[1:4]<-c("Year","country","code","value")
goods1 <- goods %>%  
  cast(Year+country~code, value = "value") %>% #From reshape package
  arrange(country,Year) 

#BOP service data in US$ million
bop<-read.csv("//DATA2/APD/Data/PIU/Export_Diversification/data_files/BOP6exp.csv",check.names = "false")

#reshape from short to long
boplong <- bop[-c(1)]%>% 
  melt(id = c("country_code","ISO","sector")) %>%
  reshape(idvar = c("country_code","ISO","variable"),timevar = "sector",direction = "wide")
colnames(boplong)[2:3]<-c("country","Year")

## Merge goods and service data (in long format)
expdata1 <- merge(goods1, boplong, by = c("Year","country"), all = TRUE)
expdata1<- arrange(expdata1,country, Year)

#write.csv(expdata1,file = "data_all.csv")

## Subset the group of LIDC and SS 
lidc<- read.csv("Q:/Data/PIU/Export_Diversification/comtrade/bycountry/lidclist.csv")
lidcl <- as.character(lidc$cty)
lidcdata<-expdata1[expdata1$country %in% lidcl,]
#write.csv(lidcdata,"lidcdata.csv")
colnames(lidcdata)[3:12]<-c("g_food","g_bev","g_crd","g_min","g_ani","g_chem","g_mfc","g_mchn","g_misl","g_others")
sapply(lidcdata,class)
lidcdata[3:12]<-sapply(lidcdata[3:12],as.numeric)
sapply(lidcdata,class)
lidctable <- as_tibble(lidcdata)
