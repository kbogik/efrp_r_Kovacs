library("readxl")
library("rstudioapi")
library("tidyr")
library(ggplot2)
library(dplyr)




#a matlabos órán használt WTI2.xlsx-et használtuk, beolvasashoz abba a mappába rakd a fájlt, ahol a scripted van
wti<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/WTI2.xlsx", sep=""))
#wti <- read_excel("WTI2.xlsx", col_types = c("date", "numeric", "numeric", "numeric")


parameters <- c("Date","CL1", "CL2","CL3","CL4","CL5","CL6","CL7","CL8","CL9","CL10","CL11","CL12","CL13","CL14","CL15","CL16","CL17","CL18","CL19","CL20","CL21","CL22","CL23","CL24")

wti$Date = as.Date(wti$Date, format="%m/%d %M:%S")
intervallum <- function(x,y){wti[wti$Date >= x & wti$Date <= y,]}

DATE1 <- as.Date("2011-04-29")
DATE2 <- as.Date("2012-05-04")

usedata <- intervallum(DATE1,DATE2) 

#usedata<- wti %>%
            #dpylr::as_tibble() %>%
            #dpylr::select(parameters)%>%
 #intervallum <- c(as.Date("2010-01-01"),as.Date("2016-12-31"))%>% 
 #lubridate::filter("Date">intervallum[1] & "Date"<intervallum[2])


colnumbers <- ncol(usedata)
rownumbers <- nrow(usedata)
lwindow <- 130
llag <- 30
runwindow <- 0
k=0

#a nem stacionaritas miatt differenciazzuk
for (asset3 in 2:rownumbers-1){
  for (asset4 in 3:colnumbers-1){
    usedata[[asset3,asset4]]<-wti[[asset3+1,asset4]]-wti[[asset3,asset4]]
  }
}

#a differenciazas miatt az utolso sort torolhetjuk
usedata<- usedata[-rownumbers,]
rownumbers <- nrow(usedata)



 eredmeny <- matrix(1,nrow = rownumbers-lwindow-llag,ncol = (length(parameters)-1)^2-(length(parameters)-1))
  
for (asset1 in 2:colnumbers) {
  for (asset2 in 2:colnumbers){
    if (asset1!=asset2){
     
       k=k+1
     
     #elnevezzük az oszlopokat
     newelem <-paste0("(",parameters[asset1],",",parameters[asset2],")")
     colnames <- c(colnames,newelem)

      #majd feltöltjük a korreláció ereményeivel
     for (runwindow in 1:(rownumbers-lwindow-llag)){
       eredmeny[runwindow,k]=cor(usedata[runwindow:(lwindow+runwindow),asset1],usedata[(runwindow+llag):(lwindow+runwindow+llag),asset2])
     }
    }
  }
}
#elneveztük a ciklusban kapott nevekre az oszlopokat
colnames(eredmeny)=colnames

parameters2 <- c(colnames)
grafikon<- eredmeny %>%
  tidyr::as_tibble() %>%
  dplyr::select(parameters2)
 
#ha random kivalasztotok egy pl.:grafikon$`(CL1,CL5)` ilyet azt kellene ábrázolni valószínűleg
#ez még csak a differencia ábrázolása, ezt írjuk át!
gg <- ggplot(usedata, aes(usedata$Date, usedata$CL1)) + 
geom_line()+
  labs(
    y="ACF", 
    x="Idő", 
    title="Keresztkorreláció", 
    caption = "Source: wti")

plot(gg)
