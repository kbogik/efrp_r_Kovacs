library("readxl")
library("tidyr")
library(ggplot2)

 %>%

wti<-readxl::read_excel(paste(dirname(getActiveDocumentContext()$path), "/WTI2.xlsx", sep=""))
#wti <- read_excel("WTI2.xlsx", col_types = c("date", "numeric", "numeric", "numeric")

View(wti)
typeof(wti$Date)
wti$date <- as.Date(wti$Date)

ccf(wti$CL1,wti$CL8,main="Cross correlation CL1 and CL8")

#paraméterek bekérésére:
windowsize <- readline(prompt="Windowsize:");
numberoflegs <- readline(prompt="Number of legs:");
commodity1 <- readline(prompt="First commodity:");
commodity2 <- readline(prompt="Second commodity:"); #itt lehet inkább vmi listát kéne egyben bekérni
startdate <- readline(prompt="Start date:");
enddate <- readline(prompt="End date:");


parameters <- c("Date","CL1", "CL2","CL3")
usedata<- wti %>%
            tidyr::as_tibble() %>%
            dplyr::select(parameters)

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


eredmeny <- matrix(1,nrow = rownumbers-lwindow-llag,ncol = 9)
  
for (asset1 in 2:colnumbers) {
  for (asset2 in 2:colnumbers){
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

#ez még csak a differencia ábrázolása
gg <- ggplot(usedata, aes(usedata$Date, usedata$CL1)) + 
geom_line()+
  labs(
    y="ACF", 
    x="Idő", 
    title="Keresztkorreláció", 
    caption = "Source: wti")

plot(gg)
