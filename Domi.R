library("readxl")
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

#sima korrelacio, ez nem is kell
correl=cor(wti[sapply( wti, is.numeric)])

# window_length=2000, erre kellene hibauzenet
#nem tudom az elejerol inditani az ablakot :(, azert van -10, 6 listaelembol áll a ccf eredmenye (erre 201-tol indulo listat ad, vagyis azelso 201 elem 0)

ij_ccf<-list(0,0,0,0,0,0)
cl_length<-nrow(wti)
cl_number<-ncol(wti)
window_length=2000
i=2
j=window_length
k=3

while (i<=(cl_number)){
 while(j <= cl_length){
  while(k <= cl_number){
    ij_ccf[j]<-(ccf(wti[j-(j-10):j,i], wti[j-(j-10):j,k]))
    k<-k+1
  }
  j <- j + 1
 i <- i + 1
}
}

##
library(ggplot2)
data("wti", package = "ggplot2")

gg <- ggplot(wti, 
plot(gg)
